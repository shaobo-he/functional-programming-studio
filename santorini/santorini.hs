module Santorini where

import Control.Monad
import System.Random
import qualified Data.HashMap.Strict as DHS (lookup, empty, HashMap, insert, adjust)
import Data.List (intercalate)
import Control.Monad.State.Lazy

type Pos = (Int, Int)
type Player = (Pos, Pos)
type Players = (Player, Player)
type Board = DHS.HashMap Pos Int

-- representation of game states
data GameState = GameState { getTurn :: Int
                           , getPlayers :: Players
                           , getBoard :: Board
                           } deriving (Show)

-- game outcome
--data GameOutcome = Win | Lose | Undecided deriving (Show)

getLevel :: Pos -> Board -> Int
getLevel pos board = case DHS.lookup pos board of
  (Just v) -> v
  Nothing -> error "pos not in board"

boardFromList :: [[Int]] -> Board
boardFromList lst = foldl insertPos (DHS.empty::Board) [(a,b) | a<-[0..4], b<-[0..4]]
  where insertPos board (a,b) = DHS.insert (a+1, b+1) ((lst !! a) !! b) board

buildAtPos :: Pos -> Board -> Board
buildAtPos = DHS.adjust (+1)

getValidAdjPosns :: Bool -> Pos -> Players -> Board -> [Pos]
getValidAdjPosns move pos (player1, player2) board = filter validPos
  [(\(x,y)->(x+ox, y+oy)) pos | ox <- [-1, 0, 1],
                                oy <- [-1, 0, 1],
                                ox /= 0 || oy /= 0] where
    validPos newPos =
      let (x,y) = newPos in
        x >=1 && x <= 5 &&
        y >= 1 && y <=5 &&
        notTaken player1 &&
        notTaken player2 &&
        notCompleteTower &&
        ((not move) || atMostHigherByOne) where
          notTaken player = (newPos /= (fst player)) && (newPos /= (snd player))
          notCompleteTower = getLevel newPos board <= 3
          atMostHigherByOne =
            let newPosH = getLevel newPos board
                posH = getLevel pos board in
                newPosH <= posH + 1

getValidAdjPosnsToMove = getValidAdjPosns True
getValidAdjPosnsToBuild = getValidAdjPosns False

--getValidMoves :: GameState -> [GameState]
getValidMoves :: GameState -> [(Players, Maybe Pos)]
getValidMoves GameState {getTurn=turn, getPlayers=players, getBoard=board} =
  do
    let player = fst players
    (pos, setter) <- zip [fst player, snd player]
                         [\pos->(pos, snd player), \pos->(fst player, pos)]
    newPos <- getValidAdjPosnsToMove pos players board
    let newPlayers = (setter newPos, snd players)
    let buildPosns = if isWinningPos board newPos
                      then [Nothing]
                      else Just <$> (getValidAdjPosnsToBuild newPos newPlayers board)
    buildPos <- buildPosns
    --return $ GameState
    --  turn
    --  newPlayers
    --  (buildAtPos buildPos board)
    return $ (newPlayers, buildPos)

isWinningPos :: Board -> Pos -> Bool
isWinningPos board pos = getLevel pos board == 3

switchPlayers :: Players -> Players
switchPlayers (a,b) = (b,a)

playOut :: Bool -> GameState -> State StdGen (Bool, String, GameState)
playOut isMyTurn gs@(GameState {getTurn=turn, getPlayers=players, getBoard=board}) =
  let moves = getValidMoves gs in
    -- can't move
    if length moves == 0
      then return (if isMyTurn then False else True, "can't move", gs)
      else
        -- pick one
        do
          chosen <- (moves !!) <$> (state $ randomR (0, (length moves) - 1))
          case snd chosen of
            Just pos -> playOut (not isMyTurn) $
                          GameState (turn+1) (switchPlayers $ fst chosen) (buildAtPos pos board)
            Nothing -> return $ (if isMyTurn then True else False, "simply win",
                        GameState (turn+1) (switchPlayers $ fst chosen) board)

main :: IO()
main = do
  let gs = GameState 2 (((2,3),(4,4)),((3,5),(2,5))) $
            boardFromList [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
  --putStrLn $ intercalate "\n" $ map show $ getValidMoves gs
  putStrLn $ show $ runState (playOut True gs) $ mkStdGen 200
