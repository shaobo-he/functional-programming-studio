{-# LANGUAGE ScopedTypeVariables #-}

-- | Time-bounded, root-parallel, anytime search driver (the IO boundary that
-- turns the pure 'Engine.Modern' search into a player move under a wall-clock
-- budget). One worker thread per capability grows its own tree until a shared
-- deadline; the trees are then combined by summed root-child visits.
module Santorini.Search
  ( timedParallel
  , workerCount
  ) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)

import Santorini.Core
import Santorini.Engine.Modern
  (Engine, MNode, advance, combineRoots, forceTreeStats, mkRoot, nodeVisits, rootSettled)

-- | Number of OS-thread capabilities the RTS was given (i.e. cores with -N).
workerCount :: IO Int
workerCount = getNumCapabilities

-- | Choose a move for @s@ with @engine@, thinking for about @thinkMs@
-- milliseconds across @threads@ independent trees (one per core).
timedParallel :: Engine -> Int -> Int -> GameState -> IO GameState
timedParallel eng thinkMs threads s = do
  now <- getCurrentTime
  let deadline = addUTCTime (fromIntegral (max 1 thinkMs) / 1000) now
      n        = max 1 threads
      seeds    = [ mkGen (i * 2654435761 + 1009) | i <- [1 .. n] ]
  mvars <- forM seeds $ \g -> do
    mv <- newEmptyMVar
    _  <- forkIO $ do
            r <- try (worker eng deadline g s)
            putMVar mv $ case r of
              Right t                  -> t
              Left (_ :: SomeException) -> mkRoot eng s
    pure mv
  trees <- mapM takeMVar mvars
  pure (combineRoots eng trees s)

-- | Grow one tree until the deadline, forcing each batch so the work happens
-- here (in the worker) rather than lazily later in the combine.
--
-- The batch size is adaptive: after each batch we measure its per-simulation
-- cost and size the next batch to about half the remaining time. A cheap engine
-- (uniform rollouts) runs large batches; an expensive one (heavy rollouts + the
-- solver) runs small ones — so both honour a tight per-move budget instead of
-- overshooting by a whole fixed 64-sim batch. Batch-first (a small initial
-- batch, run before the first clock check) still guarantees the root is
-- expanded for 'combineRoots'.
worker :: Engine -> UTCTime -> Gen -> GameState -> IO MNode
worker eng deadline g0 s = go (mkRoot eng s) g0 initialBatch
  where
    initialBatch = 8 :: Int
    go tree g batch = do
      t0 <- getCurrentTime
      let (tree', g') = runSt (advance eng batch tree) g
      _   <- evaluate (nodeVisits tree')          -- run this batch of sims here, in the worker
      now <- getCurrentTime
      if now >= deadline || rootSettled eng tree'
        then done tree'
        else go tree' g' (nextBatch batch t0 now)
    -- Aim each batch at ~half the remaining time, but cap it: a fast batch can
    -- underestimate per-sim cost, and an uncapped target would then run a giant
    -- batch that blows the budget (and trips the referee's hard limit). 64 is
    -- the original fixed batch size, which never overshot beyond the grace
    -- margin; adaptivity only ever shrinks below it for tight budgets.
    maxBatch = 64 :: Int
    nextBatch batch t0 now =
      let elapsed = realToFrac (diffUTCTime now t0) :: Double   -- seconds for `batch` sims
          perSim  = max 1e-6 (elapsed / fromIntegral (max 1 batch))
          remain  = realToFrac (diffUTCTime deadline now) :: Double
      in max 1 (min maxBatch (floor (0.5 * remain / perSim)))
    done tree = do
      _ <- evaluate (forceTreeStats tree)         -- realise child stats for combine
      pure tree
