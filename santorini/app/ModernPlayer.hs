-- | Protocol-speaking player using the modern engine (enh: negamax + solver +
-- heavy rollouts) with a time-bounded, multi-core search.
--
-- Per the Santorini protocol it takes no command-line arguments. Per-move think
-- time is read from SANTORINI_TIME_MS (default 1000 ms); it uses all RTS
-- capabilities (build with -threaded -with-rtsopts=-N). Override the rollout
-- worker count with SANTORINI_WORKERS and select an ablation with
-- SANTORINI_ENGINE when benchmarking.
module Main (main) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Santorini.Engine.Modern (enh, lookupModern)
import Santorini.Protocol (runPlayerStateIO)
import Santorini.Search (timedShared, workerCount)

main :: IO ()
main = do
  mt <- lookupEnv "SANTORINI_TIME_MS"
  mw <- lookupEnv "SANTORINI_WORKERS"
  me <- lookupEnv "SANTORINI_ENGINE"
  let thinkMs = maybe 1000 id (mt >>= readMaybe)
      engine = maybe enh id (me >>= lookupModern)
  capabilities <- workerCount
  let workers = maybe capabilities id (mw >>= readMaybe)
  runPlayerStateIO Nothing (timedShared engine thinkMs workers)
