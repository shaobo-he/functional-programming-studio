{-# LANGUAGE OverloadedStrings #-}
module Santorini (
  GameState(..),
  getValidNextStates,
  playOut,
  isWinningPlayer,
  boardFromList,
  boardToList,
  noPos,
  noPlayer,
  isDiffPos,
  isNoPlayer,
  mkRandPos,
) where

import System.Random
import qualified Data.HashMap.Strict as DHS (lookup, empty, HashMap, insert, adjust)
import Control.Monad.State.Lazy
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)

type Pos = (Int, Int)
type Player = (Pos, Pos)
type Players = (Player, Player)
type Board = DHS.HashMap Pos Int

-- representation of game states
data GameState = GameState { getTurn :: Int
                           , getPlayers :: Players
                           , getBoard :: Board
                           } deriving (Show)

noPos :: Pos
noPos = (-1, -1)

noPlayer :: Player
noPlayer = (noPos, noPos)

noPlayers :: Players
noPlayers = (noPlayer, noPlayer)

isDiffPos :: Pos -> Pos -> Bool
isDiffPos (row1, col1) (row2, col2) = not ((row1 == row2) && (col1 == col2))

isNoPlayer :: Player -> Bool
isNoPlayer (pos1, pos2) = (not $ isDiffPos noPos pos1) && (not $ isDiffPos noPos pos2)

mkRandPos :: State StdGen Pos
mkRandPos = do
  row <- state $ randomR (1,5)
  col <- state $ randomR (1,5)
  return (row, col)

-- ---------------------------------------------------------------------------
-- Protocol JSON (total parsers; encode is the inverse of decode for 0/1/2
-- placed players)
-- ---------------------------------------------------------------------------
instance FromJSON GameState where
  parseJSON (Object v) = do
    turn       <- v .: "turn"
    playersRaw <- v .: "players" :: Parser [[[Int]]]
    spaces     <- v .: "spaces"
    players    <- parsePlayers playersRaw
    return $ GameState turn players (boardFromList spaces)
    where
      parsePlayers ps = case ps of
        []        -> return noPlayers
        [p1]      -> do a <- playerListToTuple p1
                        return (a, noPlayer)
        [p1, p2]  -> do a <- playerListToTuple p1
                        b <- playerListToTuple p2
                        return (a, b)
        _         -> fail "players: expected 0, 1, or 2 players"
      playerListToTuple p = case p of
        [[x1, y1], [x2, y2]] -> return ((x1, y1), (x2, y2))
        _                    -> fail "player: expected two [row,col] workers"
  parseJSON other = typeMismatch "GameState board object" other

instance ToJSON GameState where
  toJSON (GameState turn players board) =
    object ["turn"    .= turn,
            "players" .= playersToList players,
            "spaces"  .= boardToList board]
    where
      -- Drop a player whenever EITHER worker slot is the placeholder, so a
      -- single placed player round-trips back to a one-player array.
      playersToList (player1, player2)
        | isNoPlayer player1 && isNoPlayer player2 = []
        | isNoPlayer player1                       = [playerTupleToList player2]
        | isNoPlayer player2                       = [playerTupleToList player1]
        | otherwise = [playerTupleToList player1, playerTupleToList player2]
      playerTupleToList ((x1,y1), (x2,y2)) = [[x1,y1], [x2,y2]]

getLevel :: Pos -> Board -> Int
getLevel pos board = case DHS.lookup pos board of
  (Just v) -> v
  Nothing  -> 4   -- off-board / missing cell behaves as a complete tower (wall)

boardFromList :: [[Int]] -> Board
boardFromList lst = foldl collectRow (DHS.empty::Board) $ zip lst [1..(length lst)] where
  collectRow board (row,rid) = foldl collectCol board $ zip row [1..(length row)] where
    collectCol b (level,cid) = DHS.insert (rid, cid) level b

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

getValidAdjPosnsToMove, getValidAdjPosnsToBuild :: Pos -> Players -> Board -> [Pos]
getValidAdjPosnsToMove = getValidAdjPosns True
getValidAdjPosnsToBuild = getValidAdjPosns False

getValidMoves :: GameState -> [(Players, Maybe Pos)]
getValidMoves GameState {getPlayers=players, getBoard=board} =
  do
    let player = fst players
    (pos, setter) <- zip [fst player, snd player]
                         [\p->(p, snd player), \p->(fst player, p)]
    newPos <- getValidAdjPosnsToMove pos players board
    let newPlayers = (setter newPos, snd players)
    let buildPosns = if isWinningPos board newPos
                      then [Nothing]
                      else Just <$> (getValidAdjPosnsToBuild newPos newPlayers board)
    buildPos <- buildPosns
    return $ (newPlayers, buildPos)

getValidNextStates :: GameState -> [GameState]
getValidNextStates gs@(GameState {getTurn=turn, getBoard=board}) =
  let moves = getValidMoves gs in
    updateGameStates <$> moves where
      updateGameStates (players, mpos) = case mpos of
        Just pos -> GameState (turn+1) (switchPlayers players) (buildAtPos pos board)
        Nothing  -> GameState (turn+1) (switchPlayers players) board

isWinningPos :: Board -> Pos -> Bool
isWinningPos board pos = getLevel pos board == 3

isWinningPlayer :: Board -> Player -> Bool
isWinningPlayer board player = (isWinningPos board (fst player)) ||
                                (isWinningPos board (snd player))

switchPlayers :: Players -> Players
switchPlayers (a,b) = (b,a)

-- | A uniform-random rollout that reports whether the player to move at the
-- START of the rollout (the candidate node) eventually wins. The perspective
-- is tracked by the @ref@ flag (True while the current mover equals the
-- start-mover) and is therefore independent of turn parity -- this is the fix
-- for the old "global perspective" non-adversarial bug.
playOut :: GameState -> State StdGen (Bool, GameState)
playOut start = go True start
  where
    go ref gs@(GameState turn players board)
      -- the player who just moved (now in snd) stepped onto level 3
      | isWinningPlayer board (snd players) = return (not ref, gs)
      | otherwise =
          let moves = getValidMoves gs in
          if null moves
            then return (not ref, gs)   -- current mover is stuck and loses
            else do
              chosen <- (moves !!) <$> state (randomR (0, length moves - 1))
              let nextGS = case snd chosen of
                    Just pos -> GameState (turn+1) (switchPlayers (fst chosen)) (buildAtPos pos board)
                    Nothing  -> GameState (turn+1) (switchPlayers (fst chosen)) board
              case snd chosen of
                Nothing -> return (ref, nextGS)        -- current mover made the winning step
                Just _  -> go (not ref) nextGS
