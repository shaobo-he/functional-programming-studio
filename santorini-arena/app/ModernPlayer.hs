-- | Protocol-speaking player using the modern engine (enh: negamax + solver +
-- heavy rollouts) with a time-bounded, multi-core search.
--
-- Per the Santorini protocol it takes no command-line arguments. Per-move think
-- time is read from SANTORINI_TIME_MS (default 1000 ms); it uses all RTS
-- capabilities (build with -threaded -with-rtsopts=-N).
module Main (main) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Santorini.Engine.Modern (enh)
import Santorini.Protocol (runPlayerIO)
import Santorini.Search (timedParallel, workerCount)

main :: IO ()
main = do
  mt <- lookupEnv "SANTORINI_TIME_MS"
  let thinkMs = maybe 1000 id (mt >>= readMaybe)
  threads <- workerCount
  runPlayerIO (timedParallel enh thinkMs threads)
