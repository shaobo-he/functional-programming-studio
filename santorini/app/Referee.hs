-- | Referee / driver entry point. Spawns two protocol-speaking player
-- executables and runs them against each other, logging every step.
--
-- usage: referee <playerA> <playerB> [nGames] [thinkMs] [logFile] [--boards <file>]
--
-- thinkMs is the per-move think budget handed to each player (via the
-- SANTORINI_TIME_MS env var); the referee enforces a hard limit of
-- 2*thinkMs+1000 ms per move and a too-slow player loses.
--
-- --boards <file> also writes the full board state after every ply as JSONL,
-- so later analysis needs no replay.
module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Santorini.Referee (RefConfig (..), defaultConfig, runReferee)

-- | Pull @flag value@ out of the argument list (anywhere), returning the value
-- and the remaining positional args.
extractFlag :: String -> [String] -> (Maybe String, [String])
extractFlag flag args = case break (== flag) args of
  (before, _ : val : after) -> (Just val, before ++ after)
  _                         -> (Nothing, args)

main :: IO ()
main = do
  rawArgs <- getArgs
  let (boards, args) = extractFlag "--boards" rawArgs
  case args of
    (a : b : rest) -> do
      let arg i = if length rest > i then Just (rest !! i) else Nothing
          cfg0  = defaultConfig a b
          ng    = maybe (nGames cfg0)  id (arg 0 >>= readMaybe)
          ms    = maybe (thinkMs cfg0) id (arg 1 >>= readMaybe)
          lp    = maybe (logPath cfg0) id (arg 2)
      runReferee cfg0 { nGames = ng, thinkMs = ms, logPath = lp, boardsPath = boards }
    _ -> putStrLn "usage: referee <playerA> <playerB> [nGames] [thinkMs] [logFile] [--boards <file>]"
