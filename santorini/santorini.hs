{-# LANGUAGE OverloadedStrings #-}
module Santorini (
  GameState(..),
  getValidNextStates,
  playOut,
  isWinningPlayer,
  boardFromList,
  boardToList
) where

import Control.Monad
import Control.Applicative
import System.Random
import qualified Data.HashMap.Strict as DHS (lookup, empty, HashMap, insert, adjust)
import Data.List (intercalate)
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as DBL (getContents, ByteString)

type Pos = (Int, Int)
type Player = (Pos, Pos)
type Players = (Player, Player)
type Board = DHS.HashMap Pos Int

-- representation of game states
data GameState = GameState { getTurn :: Int
                           , getPlayers :: Players
                           , getBoard :: Board
                           } deriving (Show)

-- parsing stuff
instance FromJSON GameState where
  parseJSON (Object v) =
    do
      turn <- v .: "turn"
      players <- v .: "players"
      spaces <- v .: "spaces"
      return $ GameState turn (parsePlayers players) (boardFromList spaces) where
    parsePlayers players
      | length players == 0 = (((-1,-1),(-1,-1)), ((-1,-1),(-1,-1)))
      | length players == 1 = let player = players !! 0 in
                                (((-1,-1),(-1,-1)), playerListToTuple player)
      | length players == 2 = let [player1, player2] = players in
                                (playerListToTuple player1, playerListToTuple player2) where
        playerListToTuple [[x1,y1], [x2,y2]] = ((x1,y1), (x2,y2))

instance ToJSON GameState where
  toJSON (GameState turn players board) =
    object ["turn" .= turn,
            "players" .= playersToList players,
            "spaces" .= boardToList board] where
    playersToList (((-1,-1),(-1,-1)),((-1,-1),(-1,-1))) = []
    playersToList (((-1,-1),(-1,-1)), player) = [playerTupleToList player]
    playersToList (player1, player2) = [playerTupleToList player1, playerTupleToList player2]
    playerTupleToList ((x1,y1), (x2,y2)) = [[x1,y1], [x2,y2]]

getLevel :: Pos -> Board -> Int
getLevel pos board = case DHS.lookup pos board of
  (Just v) -> v
  Nothing -> error "pos not in board"

boardFromList :: [[Int]] -> Board
boardFromList lst = foldl collectRow (DHS.empty::Board) $ zip lst [1..(length lst)] where
  collectRow board (row,rid) = foldl collectCol board $ zip row [1..(length row)] where
    collectCol board (level,cid) = DHS.insert (rid, cid) level board

boardToList :: Board -> [[Int]]
boardToList board = [[getLevel (row,col) board | col <- [1..5]] | row <- [1..5]]

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

getValidNextStates :: GameState -> [GameState]
getValidNextStates gs@(GameState {getTurn=turn, getPlayers=players, getBoard=board}) =
  let moves = getValidMoves gs in
    updateGameStates <$> moves where
      updateGameStates (players, mpos) = case mpos of
        Just pos -> GameState (turn+1) (switchPlayers players) (buildAtPos pos board)
        Nothing -> GameState (turn+1) (switchPlayers players) board

isWinningPos :: Board -> Pos -> Bool
isWinningPos board pos = getLevel pos board == 3

isWinningPlayer :: Board -> Player -> Bool
isWinningPlayer board player = (isWinningPos board (fst player)) ||
                                (isWinningPos board (snd player))

switchPlayers :: Players -> Players
switchPlayers (a,b) = (b,a)

-- should be a type class function
playOut :: GameState -> State StdGen (Bool, GameState)
playOut gs@(GameState {getTurn=turn, getPlayers=players, getBoard=board}) =
  let isMyTurn = turn `mod` 2 == 1 in
    if isWinningPlayer board $ snd players
      then return $ ((not isMyTurn), gs)
      else
        let moves = getValidMoves gs in
          -- can't move
          if length moves == 0
            then return (if isMyTurn then False else True, gs)
            else
              -- pick one
              do
                chosen <- (moves !!) <$> (state $ randomR (0, (length moves) - 1))
                case snd chosen of
                  Just pos -> playOut $
                                GameState (turn+1) (switchPlayers $ fst chosen) (buildAtPos pos board)
                  Nothing -> return $ (if isMyTurn then True else False,
                              GameState (turn+1) (switchPlayers $ fst chosen) board)

main :: IO()
main = do
  let gs = GameState 1 (((2,3),(4,4)),((3,5),(2,5))) $
            boardFromList [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
  --putStrLn $ intercalate "\n" $ map show $ getValidMoves gs
  putStrLn $ show $ runState (playOut gs) $ mkStdGen 200
