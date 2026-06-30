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
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

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
worker :: Engine -> UTCTime -> Gen -> GameState -> IO MNode
worker eng deadline g0 s = go (mkRoot eng s) g0
  where
    batch = 64 :: Int
    -- Batch-first: always run at least one batch before checking the clock, so
    -- every worker returns a tree with the root expanded (kids present) even at
    -- tiny budgets. Otherwise an all-workers-start-past-deadline race would
    -- yield only Nothing-kids trees and combineRoots would have no move.
    go tree g = do
      let (tree', g') = runSt (advance eng batch tree) g
      _   <- evaluate (nodeVisits tree')          -- run this batch of sims here, in the worker
      now <- getCurrentTime
      if now >= deadline || rootSettled eng tree'
        then done tree'
        else go tree' g'
    done tree = do
      _ <- evaluate (forceTreeStats tree)         -- realise child stats for combine
      pure tree
