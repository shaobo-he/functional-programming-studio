-- | Lightweight test suite for the Santorini engine (no external test deps;
-- exits non-zero on any failure so `cabal test` / CI catch regressions).
module Main (main) where

import System.Exit (exitFailure)

import Santorini.Core
import Santorini.Engine.Modern (chooseMove, enh)
import Santorini.Protocol (decodeBoard, encodeBoard)

-- A position where the player to move can step a worker from level 2 onto the
-- adjacent level-3 cell (1,2) and win immediately.
matePos :: GameState
matePos =
  GameState 1 (((1, 1), (5, 5)), ((3, 3), (3, 4)))
    (setLevels [((1, 1), 2), ((1, 2), 3)] emptyBoard)

openingPos :: GameState
openingPos = GameState 1 (((2, 2), (4, 4)), ((2, 4), (4, 2))) emptyBoard

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ name)
  pure ok

main :: IO ()
main = do
  let -- the modern engine (with the solver) must find the mate-in-1
      mateResult = fst (runSt (chooseMove enh 200 matePos) (mkGen 1))
      -- the allocation-free sampler must enumerate exactly getValidMoves
      samplerOK = getValidMoves openingPos
                    == [ nthMove openingPos k | k <- [0 .. moveCount openingPos - 1] ]
      -- the protocol JSON must round-trip a board
      roundTripOK = case decodeBoard (encodeBoard openingPos) of
        Just gs -> getPlayers gs == getPlayers openingPos
                   && boardToList (getBoard gs) == boardToList (getBoard openingPos)
                   && getTurn gs == getTurn openingPos
        Nothing -> False
      -- every legal successor advances the turn by one
      turnsOK = all (\t -> getTurn t == getTurn openingPos + 1)
                    (getValidNextStates openingPos)
      -- the opening has legal moves and the counts agree
      countOK = moveCount openingPos == length (getValidMoves openingPos)
                && moveCount openingPos > 0

  results <- sequence
    [ check "modern (enh) finds mate-in-1"       (moverWon mateResult)
    , check "sampler nthMove == getValidMoves"   samplerOK
    , check "protocol board JSON round-trips"     roundTripOK
    , check "successors increment the turn"       turnsOK
    , check "moveCount == length getValidMoves"   countOK
    ]
  if and results then putStrLn "ALL PASS" else exitFailure
