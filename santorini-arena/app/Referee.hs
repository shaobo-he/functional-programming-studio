-- | Referee / driver entry point. Spawns two protocol-speaking player
-- executables and runs them against each other, logging every step.
--
-- usage: referee <playerA> <playerB> [nGames] [thinkMs] [logFile]
--
-- thinkMs is the per-move think budget handed to each player (via the
-- SANTORINI_TIME_MS env var); the referee enforces a hard limit of
-- 2*thinkMs+1000 ms per move and a too-slow player loses.
module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Santorini.Referee (RefConfig (..), defaultConfig, runReferee)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (a : b : rest) -> do
      let arg i  = if length rest > i then Just (rest !! i) else Nothing
          cfg0   = defaultConfig a b
          ng     = maybe (nGames cfg0)  id (arg 0 >>= readMaybe)
          ms     = maybe (thinkMs cfg0) id (arg 1 >>= readMaybe)
          lp     = maybe (logPath cfg0) id (arg 2)
      runReferee cfg0 { nGames = ng, thinkMs = ms, logPath = lp }
    _ -> putStrLn "usage: referee <playerA> <playerB> [nGames] [thinkMs] [logFile]"
