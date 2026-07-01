{-# LANGUAGE ScopedTypeVariables #-}

-- | Time-bounded WU-UCT-style shared-tree search. One coordinator owns the
-- persistent tree and performs selection/backpropagation sequentially. Worker
-- threads only evaluate pure rollouts, so no tree node is concurrently mutated.
module Santorini.Search
  ( timedParallel
  , timedShared
  , workerCount
  ) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (forM, replicateM_)
import Data.List (foldl')
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import Santorini.Core
import Santorini.Engine.Modern
  ( Engine
  , MNode
  , RolloutJob
  , cancelRollout
  , chooseRootChild
  , finishRollout
  , forceTreeStats
  , nodeState
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

-- | Search one position using a shared logical tree. A cached root is reused
-- when it matches the position or contains the opponent's reply as a child.
timedShared
  :: Engine
  -> Int
  -> Int
  -> Maybe MNode
  -> GameState
  -> IO (GameState, Maybe MNode)
timedShared eng thinkMs workers cached state = do
  let root = reuseRoot eng state cached
  searched <- growShared eng thinkMs workers (stateSeed state) root
  case chooseRootChild eng searched of
    Just child -> pure (nodeState child, Just child)
    Nothing -> case getValidNextStates state of
      next : _ -> pure (next, Nothing)
      [] -> pure (state, Nothing)

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
