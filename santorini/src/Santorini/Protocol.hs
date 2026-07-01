{-# LANGUAGE OverloadedStrings #-}

-- | The Utah CS6963 Santorini line-delimited JSON protocol.
--
-- Each message is one line of JSON, self-delimited by @{ }@ (a board state) or
-- @[ ]@ (a placement array). Board: @{turn, players, spaces}@ with 1-based
-- @[row,col]@ coordinates and levels @0..4@. Placement handshake: the first
-- player receives @[]@ and returns @[[w1,w2]]@; the second receives
-- @[[opp1,opp2]]@ and returns @[[opp1,opp2],[own1,own2]]@ (opponent first).
--
-- This module is the IO/serialisation boundary; the engines stay pure.
module Santorini.Protocol
  ( -- * Serialisation
    encodeBoard
  , decodeBoard
  , decodePlacement
  , encodePlacement
  , placementToPlayers
  , playerToList
    -- * Placement
  , placeWorkers
    -- * Running a protocol-speaking player
  , runPlayer
  , runPlayerIO
  , runPlayerStateIO
  ) where

import Data.Aeson (Value, decode, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (nub)
import System.IO (BufferMode (LineBuffering), hFlush, hSetBuffering, isEOF, stdin, stdout)

import Santorini.Core

-- ----------------------------------------------------------------------------
-- Board <-> JSON
-- ----------------------------------------------------------------------------

posToList :: Pos -> [Int]
posToList (r, c) = [r, c]

playerToList :: Player -> [[Int]]
playerToList (w1, w2) = [posToList w1, posToList w2]

-- | Encode a board state as one JSON line.
encodeBoard :: GameState -> BL.ByteString
encodeBoard (GameState turn (p1, p2) board) =
  encode $ object
    [ "turn"    .= turn
    , "players" .= [playerToList p1, playerToList p2]
    , "spaces"  .= boardToList board
    ]

-- | Decode a board state from one JSON line (a @{...}@ object). Total: returns
-- 'Nothing' on anything that is not a well-formed 2-player board.
decodeBoard :: BL.ByteString -> Maybe GameState
decodeBoard bs = decode bs >>= parseMaybe parseBoard

parseBoard :: Value -> Parser GameState
parseBoard = withObject "board" $ \o -> do
  turn    <- o .: "turn"
  players <- o .: "players" :: Parser [[[Int]]]
  spaces  <- o .: "spaces"  :: Parser [[Int]]
  pls     <- parsePlayers players
  pure (GameState turn pls (boardFromList spaces))

parsePlayers :: [[[Int]]] -> Parser Players
parsePlayers ps = case ps of
  [a, b] -> (,) <$> toPlayer a <*> toPlayer b
  _      -> fail "board: expected exactly two players"

toPlayer :: [[Int]] -> Parser Player
toPlayer [[r1, c1], [r2, c2]] = pure ((r1, c1), (r2, c2))
toPlayer _                    = fail "player: expected two [row,col] workers"

-- ----------------------------------------------------------------------------
-- Placement arrays <-> JSON
-- ----------------------------------------------------------------------------

-- | Decode a placement message (@[]@, one player, or two players). Returns
-- 'Nothing' for a board object, so the player loop can dispatch on the result.
decodePlacement :: BL.ByteString -> Maybe [[[Int]]]
decodePlacement bs = decode bs :: Maybe [[[Int]]]

encodePlacement :: [[[Int]]] -> BL.ByteString
encodePlacement = encode

-- | Interpret a placement array as a list of fully-placed players.
placementToPlayers :: [[[Int]]] -> Maybe [Player]
placementToPlayers = mapM toPos
  where
    toPos [[r1, c1], [r2, c2]] = Just ((r1, c1), (r2, c2))
    toPos _                    = Nothing

-- ----------------------------------------------------------------------------
-- Placement strategy
-- ----------------------------------------------------------------------------

-- | Place our two workers on distinct unoccupied cells, preferring central
-- squares. Deterministic; the 'St' threading is kept only so callers can stay
-- uniform with the move path.
placeWorkers :: [Pos] -> St Gen (Pos, Pos)
placeWorkers occupied =
  case filter (`notElem` occupied) preference of
    (a : b : _) -> pure (a, b)
    _           -> pure ((1, 1), (1, 2))   -- unreachable on a 5x5 board
  where
    preference = nub $ [(2, 2), (4, 4), (2, 4), (4, 2), (3, 3)]
                       ++ [ (r, c) | r <- [1 .. 5], c <- [1 .. 5] ]

-- ----------------------------------------------------------------------------
-- Player loop
-- ----------------------------------------------------------------------------

-- | Respond to a placement message (pure; placement needs no search time).
handlePlacement :: [[[Int]]] -> St Gen BL.ByteString
handlePlacement players = case players of
  [] -> do
    (w1, w2) <- placeWorkers []
    pure (encodePlacement [playerToList (w1, w2)])
  (opp : _) -> do
    let occ = case placementToPlayers [opp] of
                Just [(o1, o2)] -> [o1, o2]
                _               -> []
    (w1, w2) <- placeWorkers occ
    pure (encodePlacement [opp, playerToList (w1, w2)])

initStdio :: IO ()
initStdio = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering

-- | Drive a pure chooser as a protocol-speaking player: read a line, dispatch
-- on placement vs board, write one line back, flush, repeat until EOF.
runPlayer :: (GameState -> St Gen GameState) -> IO ()
runPlayer chooser = runPlayerIO (\s -> pure (fst (runSt (chooser s) (mkGen 1))))

-- | Like 'runPlayer' but the board-move decision runs in 'IO' (so it can do a
-- wall-clock-bounded, multi-threaded search). Placement stays pure.
runPlayerIO :: (GameState -> IO GameState) -> IO ()
runPlayerIO choose =
  runPlayerStateIO () $ \() state -> do
    next <- choose state
    pure (next, ())

-- | Like 'runPlayerIO', but retain search state between board messages. The
-- placement handshake does not modify that state.
runPlayerStateIO :: search -> (search -> GameState -> IO (GameState, search)) -> IO ()
runPlayerStateIO initialSearch choose = do
  initStdio
  go (mkGen 20240601) initialSearch
  where
    go g search = do
      eof <- isEOF
      if eof
        then pure ()
        else do
          line <- getLine
          let bs = BLC.pack line
          case decodePlacement bs of
            Just players -> do
              let (resp, g') = runSt (handlePlacement players) g
              BLC.putStrLn resp
              hFlush stdout
              go g' search
            Nothing -> case decodeBoard bs of
              Just s -> do
                (s', search') <- choose search s
                BLC.putStrLn (encodeBoard s')
                hFlush stdout
                go g search'
              Nothing -> go g search   -- unparseable line: skip without crashing
