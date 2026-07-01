-- | Protocol-speaking player using the modern engine (enh: negamax + solver +
-- heavy rollouts) with a time-bounded, multi-core search.
--
-- Per the Santorini protocol it takes no command-line arguments. Per-move think
-- time is read from SANTORINI_TIME_MS (default 1000 ms); it uses all RTS
-- capabilities (build with -threaded -with-rtsopts=-N). Override the rollout
-- worker count with SANTORINI_WORKERS, the root-parallel coordinator count with
-- SANTORINI_COORDINATORS (default workers/6; 1 = single shared tree), and select
-- an ablation with SANTORINI_ENGINE when benchmarking.
module Main (main) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Santorini.Engine.Modern (enh, lookupModern)
import Santorini.Protocol (runPlayerStateIO)
import Santorini.Search (timedShared, timedSharedWith, workerCount)

main :: IO ()
main = do
  mt <- lookupEnv "SANTORINI_TIME_MS"
  mw <- lookupEnv "SANTORINI_WORKERS"
  mc <- lookupEnv "SANTORINI_COORDINATORS"
  me <- lookupEnv "SANTORINI_ENGINE"
  let thinkMs = maybe 1000 id (mt >>= readMaybe)
      engine = maybe enh id (me >>= lookupModern)
  capabilities <- workerCount
  let workers = maybe capabilities id (mw >>= readMaybe)
      choose = case mc >>= readMaybe of
        Just coords -> timedSharedWith engine thinkMs workers coords
        Nothing     -> timedShared engine thinkMs workers
  runPlayerStateIO Nothing choose
