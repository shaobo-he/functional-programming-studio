{-# LANGUAGE ScopedTypeVariables #-}

-- | Time-bounded WU-UCT-style shared-tree search. One coordinator owns the
-- persistent tree and performs selection/backpropagation sequentially. Worker
-- threads only evaluate pure rollouts, so no tree node is concurrently mutated.
module Santorini.Search
  ( timedParallel
  , timedShared
  , timedSharedWith
  , workerCount
  ) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (forM, replicateM_)
import Data.Function (on)
import Data.List (foldl', maximumBy)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import Santorini.Core
import Santorini.Engine.Modern
  ( Engine
  , MNode
  , RolloutJob
  , cancelRollout
  , childrenOf
  , chooseRootChild
  , finishRollout
  , forceTreeStats
  , mergeRoots
  , mkRoot
  , nodeState
  , nodeVisits
  , reserveRollout
  , reuseRoot
  , rootSettled
  , runRolloutJob
  )

data Work = Run !RolloutJob | Stop
type Result = (RolloutJob, Either SomeException Int)

-- | Number of OS-thread capabilities the RTS was given. Benchmarking may show
-- that a CPU-heavy rollout policy is better with fewer workers than SMT threads.
workerCount :: IO Int
workerCount = getNumCapabilities

-- | Compatibility wrapper for stateless callers. Production uses 'timedShared'
-- so the selected subtree survives across protocol turns.
timedParallel :: Engine -> Int -> Int -> GameState -> IO GameState
timedParallel eng thinkMs workers state =
  fst <$> timedShared eng thinkMs workers Nothing state

-- | Search one position. Below the single-coordinator saturation point the
-- search is one shared logical tree (a cached root is reused when it matches the
-- position or holds the opponent's reply). Above it we run several independent
-- coordinators and merge their roots (root parallelism), which keeps more cores
-- busy than one serial coordinator can feed.
timedShared
  :: Engine
  -> Int
  -> Int
  -> Maybe MNode
  -> GameState
  -> IO (GameState, Maybe MNode)
timedShared eng thinkMs workers =
  timedSharedWith eng thinkMs workers (max 1 (workers `div` workersPerCoordinator))

-- | Like 'timedShared' but with an explicit root-parallel degree (coordinator
-- count, e.g. from SANTORINI_COORDINATORS). @coords <= 1@ is the single shared
-- tree; higher values run that many independent coordinators over
-- @workers `div` coords@ rollout workers each.
timedSharedWith
  :: Engine -> Int -> Int -> Int -> Maybe MNode -> GameState -> IO (GameState, Maybe MNode)
timedSharedWith eng thinkMs workers coords cached state
  | coords <= 1 = singleShared eng thinkMs workers cached state
  | otherwise   = rootParallel eng thinkMs coords perCoord cached state
  where
    perCoord = max 1 (workers `div` coords)

-- | One coordinator saturates roughly this many rollout workers before its
-- serial selection/backup caps throughput (measured ~6 on a 12-core box).
workersPerCoordinator :: Int
workersPerCoordinator = 6

-- | The original single-shared-tree driver.
singleShared
  :: Engine -> Int -> Int -> Maybe MNode -> GameState -> IO (GameState, Maybe MNode)
singleShared eng thinkMs workers cached state = do
  let root = reuseRoot eng state cached
  searched <- growShared eng thinkMs workers (stateSeed state) root
  case chooseRootChild eng searched of
    Just child -> pure (nodeState child, Just child)
    Nothing    -> firstMove state

-- | Root parallelism: @coords@ independent coordinators, each with its own tree
-- and @perCoord@ rollout workers, run concurrently to the deadline; then root
-- statistics are summed and exact proofs OR-combined to choose the move. The
-- deepest matching subtree is retained for next-turn reuse.
rootParallel
  :: Engine -> Int -> Int -> Int -> Maybe MNode -> GameState -> IO (GameState, Maybe MNode)
rootParallel eng thinkMs coords perCoord cached state = do
  let seed0 = stateSeed state
      roots = [ if i == 0 then reuseRoot eng state cached else mkRoot eng state
              | i <- [0 .. coords - 1] ]
      seeds = [ seed0 + i * 2654435761 + 1 | i <- [0 .. coords - 1] ]
  searched <- runConcurrently
                [ growShared eng thinkMs perCoord seed root
                | (seed, root) <- zip seeds roots ]
  case chooseRootChild eng (mergeRoots searched) of
    Just child ->
      let moveState = nodeState child
      in pure (moveState, bestSubtreeFor moveState searched)
    Nothing -> firstMove state

firstMove :: GameState -> IO (GameState, Maybe MNode)
firstMove state = case getValidNextStates state of
  next : _ -> pure (next, Nothing)
  []       -> pure (state, Nothing)

-- | The deepest real subtree for @moveState@ across the searched trees, kept so
-- the next protocol turn can reuse accumulated search under the chosen move.
bestSubtreeFor :: GameState -> [MNode] -> Maybe MNode
bestSubtreeFor moveState roots =
  case [ c | r <- roots, c <- childrenOf r, nodeState c == moveState ] of
    [] -> Nothing
    cs -> Just (maximumBy (compare `on` nodeVisits) cs)

-- | Run IO actions concurrently (one thread each) and collect their results.
runConcurrently :: [IO a] -> IO [a]
runConcurrently acts = do
  slots <- forM acts $ \act -> do
    slot <- newEmptyMVar
    _ <- forkIO (act >>= putMVar slot)
    pure slot
  mapM takeMVar slots

growShared :: Engine -> Int -> Int -> Int -> MNode -> IO MNode
growShared eng thinkMs requestedWorkers seed root = do
  jobs <- newChan
  results <- newChan
  let n = max 1 requestedWorkers
      workerSeeds = [mkGen (seed + i * 2654435761 + 1009) | i <- [1 .. n]]
  done <- forM workerSeeds $ \workerSeed -> do
    stopped <- newEmptyMVar
    _ <- forkIO (rolloutWorker eng workerSeed jobs results stopped)
    pure stopped
  start <- getCurrentTime
  let deadline = addUTCTime (fromIntegral (max 1 thinkMs) / 1000) start
  searched <- coordinate deadline n jobs results root (mkGen (seed + 104729)) 0
  replicateM_ n (writeChan jobs Stop)
  mapM_ takeMVar done
  _ <- evaluate (forceTreeStats searched)
  pure searched
  where
    coordinate
      :: UTCTime
      -> Int
      -> Chan Work
      -> Chan Result
      -> MNode
      -> Gen
      -> Int
      -> IO MNode
    coordinate deadline capacity jobs results tree selectionGen inFlight = do
      now <- getCurrentTime
      if inFlight < capacity && now < deadline && not (rootSettled eng tree)
        then do
          let ((job, tree'), selectionGen') = runSt (reserveRollout eng tree) selectionGen
          case job of
            Just rolloutJob -> do
              -- No per-reserve forceTreeStats: the strict node stats are forced
              -- once per completed rollout in drainOrFinish, and the next
              -- reserve's selectChild descent forces the freshly reserved path,
              -- so forcing here is redundant serial work on the coordinator.
              writeChan jobs (Run rolloutJob)
              coordinate deadline capacity jobs results tree' selectionGen' (inFlight + 1)
            Nothing -> drainOrFinish deadline capacity jobs results tree' selectionGen' inFlight
        else drainOrFinish deadline capacity jobs results tree selectionGen inFlight

    drainOrFinish deadline capacity jobs results tree selectionGen inFlight
      | inFlight == 0 = pure tree
      | otherwise = do
          (job, outcome) <- readChan results
          let tree' = case outcome of
                Right value -> finishRollout eng job value tree
                Left _ -> cancelRollout job tree
          _ <- evaluate (forceTreeStats tree')
          coordinate deadline capacity jobs results tree' selectionGen (inFlight - 1)

rolloutWorker
  :: Engine
  -> Gen
  -> Chan Work
  -> Chan Result
  -> MVar ()
  -> IO ()
rolloutWorker eng = go
  where
    go gen jobs results stopped =
      loop gen `finally` putMVar stopped ()
      where
        loop currentGen = do
          work <- readChan jobs
          case work of
            Stop -> pure ()
            Run job -> do
              let (value, nextGen) = runSt (runRolloutJob eng job) currentGen
              outcome <- try (evaluate value)
              writeChan results (job, outcome)
              loop nextGen

-- Keep searches reproducible while avoiding the same random streams on every
-- turn. Equal positions still receive equal streams, which is useful in tests.
stateSeed :: GameState -> Int
stateSeed (GameState turn ((a, b), (c, d)) board) =
  foldl' mix turn (concatMap pos [a, b, c, d] ++ concat (boardToList board))
  where
    pos (r, col) = [r, col]
    -- The 32-bit FNV prime is only a convenient odd mixing multiplier here;
    -- this is not intended to be the FNV hash algorithm.
    mix h x = h * seedMixPrime + x

seedMixPrime :: Int
seedMixPrime = 16777619
