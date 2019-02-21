module Main where

import Control.Monad.State.Lazy
import Control.Monad
import Data.Aeson
import System.Random
import System.IO (stdin, stdout, hFlush)
import qualified Data.ByteString as B (hGetLine, ByteString)
import qualified Data.ByteString.Lazy as BL (putStrLn, ByteString)
import Santorini
import MCTS

updateTurn :: Int -> GameState -> GameState
updateTurn newTurn (GameState turn players board) =
  GameState newTurn players board

-- get new player positions, essentially rejection sampling
{--
placePlayer1 :: GameState -> State StdGen GameState
placePlayer1 gs@(GameState turn players board) = do
  pos1 <- mkRandPos
  pos2 <- mkRandPos
  if isDiffPos pos1 pos2
    then return $ GameState turn (noPlayer, (pos1, pos2)) board
    else placePlayer1 gs
--}

{--
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
--}

{--
start :: GameState -> State StdGen GameState
start gs@(GameState turn players@(player1, player2) board)
  | isNoPlayer player1 && isNoPlayer player2 = placePlayer1 gs
  | isNoPlayer player2 = placePlayer2 gs
  | otherwise = mcts (GameState 1 players board)
--}
tupleToList :: (Int, Int) -> [Int]
tupleToList (x,y) = [x,y]

listToTuple :: [Int] -> (Int, Int)
listToTuple [x,y] = (x,y)

placePlayer1' :: State StdGen [[[Int]]]
placePlayer1' = do
  pos1 <- mkRandPos
  pos2 <- mkRandPos
  if isDiffPos pos1 pos2
    then return $ [[tupleToList pos1, tupleToList pos2]]
    else placePlayer1'

placePlayer2' :: [[[Int]]] -> State StdGen [[[Int]]]
placePlayer2' lst@[[pos1, pos2], [pos1', pos2']]
  | isDiffPos noPos (listToTuple pos1') && isDiffPos noPos (listToTuple pos2') = return lst
  -- pos1' is not done, get it working
  | not $ isDiffPos noPos (listToTuple pos1') = do
    pos <- mkRandPos
    if (isDiffPos pos (listToTuple pos1)) && (isDiffPos pos (listToTuple pos2))
      then placePlayer2' $ [[pos1, pos2], [tupleToList pos, pos2']]
      else placePlayer2' lst
  -- pos2' is not done, switch pos1' and pos2'
  | not $ isDiffPos noPos (listToTuple pos2') = placePlayer2' $ [[pos1, pos2], [pos2', pos1']]

placePlayer :: B.ByteString -> State StdGen (Maybe BL.ByteString)
placePlayer jsonStr = case decodeStrict jsonStr :: Maybe [[[Int]]] of
  Just players -> case players of 
                    [] -> Just . encode <$> placePlayer1'
                    [player] -> Just . encode <$> placePlayer2' [player, [[-1,-1],[-1,-1]]]
  Nothing -> return Nothing

playBoard :: B.ByteString -> State StdGen BL.ByteString
playBoard jsonStr =
  case decodeStrict jsonStr of
    Just gs@(GameState turn players board) -> encode . (updateTurn (turn+1)) <$>  mcts gs 
    Nothing -> error "unable to play board!"

play :: StdGen -> IO () 
play gen0 = do
  jsonStr <- B.hGetLine stdin
  let (playerJsonStr, gen1) = runState (placePlayer jsonStr) gen0
  case playerJsonStr of
    Just str -> BL.putStrLn str >> hFlush stdout >> play gen1
    Nothing -> let (board, gen2) = runState (playBoard jsonStr) gen1 in BL.putStrLn board >> hFlush stdout >> play gen2

main :: IO ()
main = do
  --jsonStr <- DBL.getContents
  --jsonStr <- B.hGetLine stdin
  --case decodeStrict jsonStr of
  --  Just gs@(GameState turn players board) -> BL.putStrLn $ encode . (updateTurn (turn+1)) . fst $ runState (start gs) $ mkStdGen 200
  --  Nothing -> placePlayer jsonStr
  --main
  play $ mkStdGen 200
