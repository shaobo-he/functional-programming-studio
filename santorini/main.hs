module Main where

import Control.Monad.State.Lazy
import Data.Aeson
import System.Random
import qualified Data.ByteString.Lazy as DBL (getContents, ByteString)
import Santorini
import MCTS

updateTurn :: Int -> GameState -> GameState
updateTurn newTurn (GameState turn players board) =
  GameState newTurn players board

-- get new player positions, essentially rejection sampling
placePlayer1 :: GameState -> State StdGen GameState
placePlayer1 gs@(GameState turn players board) = do
  pos1 <- mkRandPos
  pos2 <- mkRandPos
  if isDiffPos pos1 pos2
    then return $ GameState turn (noPlayer, (pos1, pos2)) board
    else placePlayer1 gs

placePlayer2 :: GameState -> State StdGen GameState
placePlayer2 gs@(GameState turn (player@(pos1, pos2), (pos1', pos2')) board)
  | isDiffPos noPos pos1' && isDiffPos noPos pos2' = return gs
  -- pos1' is not done, get it working
  | not $ isDiffPos noPos pos1' = do
    pos <- mkRandPos
    if (isDiffPos pos pos1) && (isDiffPos pos pos2)
      then placePlayer2 $ GameState turn (player, (pos, pos2')) board
      else placePlayer2 gs
  -- pos2' is not done, switch pos1' and pos2'
  | not $ isDiffPos noPos pos2' = placePlayer2 $ GameState turn (player, (pos2', pos1')) board

start :: GameState -> State StdGen GameState
start gs@(GameState turn players@(player1, player2) board)
  | isNoPlayer player1 && isNoPlayer player2 = placePlayer1 gs
  | isNoPlayer player2 = placePlayer2 gs
  | otherwise = mcts (GameState 1 players board)

main :: IO ()
main = do
  jsonStr <- DBL.getContents
  case decode jsonStr of
    Just gs@(GameState turn players board) -> putStrLn $ show $ encode . (updateTurn (turn+1)) . fst $ runState (start gs) $ mkStdGen 200
    Nothing -> putStrLn $ show jsonStr
  main
