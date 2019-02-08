module Santorini where

import Control.Monad
import System.Random
import qualified Data.HashMap.Strict as DHS (lookup, empty, HashMap, insert, adjust)

type Pos = (Int, Int)
type Player = (Pos, Pos)
type Players = (Player, Player)
type Board = DHS.HashMap Pos Int

data GameState = GameState { getTurn :: Int
                           , getPlayers :: Players
                           , getBoard :: Board
                           } deriving (Show)

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

getValidMoves :: GameState -> [GameState]
getValidMoves GameState {getTurn=turn, getPlayers=players, getBoard=board} =
  do
    let player = fst players
    (pos, setter) <- zip [fst player, snd player]
                         [\pos->(pos, snd player), \pos->(fst player, pos)]
    newPos <- getValidAdjPosnsToMove pos players board
    let newPlayers = (setter newPos, snd players)
    let buildPosns = getValidAdjPosnsToBuild newPos newPlayers board
    guard $ length buildPosns > 0
    buildPos <- buildPosns
    return $ GameState
      turn
      newPlayers
      (buildAtPos buildPos board)

main :: IO()
main = do
  let gs = GameState 2 (((2,3),(4,4)),((3,5),(2,5))) $ boardFromList [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
  putStrLn $ show $ length $ getValidMoves gs
