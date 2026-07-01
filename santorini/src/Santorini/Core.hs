{-# LANGUAGE NamedFieldPuns #-}

-- | Shared Santorini game model used by every engine in the arena.
--
-- This is the single source of truth for the rules, the board representation,
-- the (self-contained) RNG, and the one move sampler that both the @Modern@ and
-- @Legacy@ engines build on.  Keeping the model in one place is what lets two
-- different MCTS implementations play each other: the arena hands each engine a
-- 'GameState' and applies whatever 'GameState' it returns.
module Santorini.Core
  ( -- * Self-contained State monad + RNG (no mtl / random needed)
    St(..)
  , stTake
  , Gen
  , mkGen
  , nextW
  , randomR
    -- * Game model
  , Pos
  , Player
  , Players
  , Board
  , GameState(..)
  , pix
  , getLevel
  , boardFromList
  , boardToList
  , buildAtPos
  , emptyBoard
  , setLevels
    -- * Rules: adjacency / moves
  , getValidAdjPosns
  , getValidAdjPosnsToMove
  , getValidAdjPosnsToBuild
  , validAdj
  , adjOffsets
  , countAdj
  , nthAdj
  , isWinningPos
  , isWinningPlayer
  , switchPlayers
  , moverWon
  , hasWinningMove
    -- * Move enumeration + the single allocation-free sampler
  , getValidMoves
  , getValidNextStates
  , Move
  , moveEntries
  , slotsFor
  , moveCount
  , nthMove
  , applyMove
    -- * Rollouts shared by the engines
  , Policy
  , randomMove
  , uniformPick
  , rollout
  , rolloutUniform
  ) where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.List (foldl')
import Data.Word (Word64)

-- ----------------------------------------------------------------------------
-- Minimal State monad (avoids mtl) and a small deterministic RNG (avoids random)
-- ----------------------------------------------------------------------------

newtype St s a = St { runSt :: s -> (a, s) }

instance Functor (St s) where
  fmap f (St g) = St $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (St s) where
  pure a = St $ \s -> (a, s)
  (St f) <*> (St g) = St $ \s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2)

instance Monad (St s) where
  (St g) >>= k = St $ \s -> let (a, s') = g s in runSt (k a) s'

stTake :: (s -> (a, s)) -> St s a
stTake = St

type Gen = Word

-- | Build a generator from any seed (always non-zero so the LCG never sticks).
mkGen :: Int -> Gen
mkGen = fromIntegral . (+ 1) . abs

-- | One LCG step (Knuth/MMIX constants); good enough for uniform sampling here.
nextW :: Gen -> (Word, Gen)
nextW g = let g' = g * 6364136223846793005 + 1442695040888963407 in (g', g')

-- | Uniform integer in @[lo, hi]@ (inclusive).
randomR :: (Int, Int) -> Gen -> (Int, Gen)
randomR (lo, hi) g =
  let (w, g') = nextW g
      n = hi - lo + 1
  in (lo + fromIntegral (w `mod` fromIntegral (max 1 n)), g')

-- ----------------------------------------------------------------------------
-- Game model
-- ----------------------------------------------------------------------------

type Pos = (Int, Int)
type Player = (Pos, Pos)            -- ^ a player's two workers
type Players = (Player, Player)     -- ^ (player to move, opponent)
-- | The 5x5 board packed into two 64-bit words: 25 cells x 3 bits (levels
-- 0..4). Cells 0..20 live in the low word (21*3 = 63 bits), cells 21..24 in the
-- high word (4*3 = 12 bits). This replaces a pointer-heavy @IntMap Int@ with a
-- flat, allocation-cheap value whose equality is two word compares.
data Board = Board !Word64 !Word64 deriving (Eq, Show)

-- | Row/col (1..5) to a dense 0..24 board index.
pix :: Pos -> Int
pix (r, c) = (r - 1) * 5 + (c - 1)

data GameState = GameState
  { getTurn    :: Int
  , getPlayers :: Players
  , getBoard   :: Board
  } deriving (Eq, Show)

-- Low-level 3-bit cell access on the packed board. @i@ is a dense 0..24 index;
-- callers of setCell/cellAt guarantee that range (getLevel guards it). Cells
-- 0..20 sit at bit @i*3@ of the low word, 21..24 at bit @(i-21)*3@ of the high
-- word, so the widest shift is 60 and never overflows a 64-bit word.
cellAt :: Int -> Board -> Int
cellAt i (Board lo hi)
  | i <= 20   = fromIntegral ((lo `shiftR` (i * 3)) .&. 7)
  | otherwise = fromIntegral ((hi `shiftR` ((i - 21) * 3)) .&. 7)

setCell :: Int -> Int -> Board -> Board
setCell i v (Board lo hi)
  | i <= 20   = Board ((lo .&. complement (7 `shiftL` s)) .|. (w `shiftL` s)) hi
  | otherwise = Board lo ((hi .&. complement (7 `shiftL` t)) .|. (w `shiftL` t))
  where
    s = i * 3
    t = (i - 21) * 3
    w = fromIntegral v :: Word64

-- | Tower level at a position. The board is always full (25 cells), so an
-- out-of-range query (an off-board neighbour) is treated as a complete-tower
-- wall rather than crashing, which keeps every caller total.
getLevel :: Pos -> Board -> Int
getLevel pos board = let i = pix pos in if i < 0 || i > 24 then 4 else cellAt i board

boardFromList :: [[Int]] -> Board
boardFromList lst = foldl collectRow emptyBoard $ zip lst [1 ..]
  where
    collectRow board (row, rid) = foldl collectCol board $ zip row [1 ..]
      where collectCol b (level, cid) = setCell (pix (rid, cid)) level b

-- | The board as a 5x5 row-major list of levels (the protocol \"spaces\" field).
boardToList :: Board -> [[Int]]
boardToList board = [ [ getLevel (r, c) board | c <- [1 .. 5] ] | r <- [1 .. 5] ]

buildAtPos :: Pos -> Board -> Board
buildAtPos pos b = let i = pix pos in setCell i (cellAt i b + 1) b

emptyBoard :: Board
emptyBoard = Board 0 0

setLevels :: [(Pos, Int)] -> Board -> Board
setLevels xs b = foldl (\acc (p, l) -> setCell (pix p) l acc) b xs

-- ----------------------------------------------------------------------------
-- Rules
-- ----------------------------------------------------------------------------

-- | Legal neighbours of @pos@. @move=True@ adds the climb-at-most-one rule
-- (used for the worker step); @move=False@ is the build rule.
getValidAdjPosns :: Bool -> Pos -> Players -> Board -> [Pos]
getValidAdjPosns move pos pls board =
  filter (validAdj move pos pls board)
    [ (x + ox, y + oy) | let (x, y) = pos, (ox, oy) <- adjOffsets ]

getValidAdjPosnsToMove, getValidAdjPosnsToBuild :: Pos -> Players -> Board -> [Pos]
getValidAdjPosnsToMove  = getValidAdjPosns True
getValidAdjPosnsToBuild = getValidAdjPosns False

-- | The 8 king-move offsets, in a fixed order. Both the list enumeration
-- ('getValidAdjPosns') and the index sampler ('nthAdj') walk this list, so
-- sampling by index lands on exactly the same neighbour as the list path.
adjOffsets :: [(Int, Int)]
adjOffsets = [ (ox, oy) | ox <- [-1, 0, 1], oy <- [-1, 0, 1], ox /= 0 || oy /= 0 ]

validAdj :: Bool -> Pos -> Players -> Board -> Pos -> Bool
validAdj move pos (player1, player2) board newPos =
  let (x, y) = newPos
      notTaken pl = newPos /= fst pl && newPos /= snd pl
  in x >= 1 && x <= 5 && y >= 1 && y <= 5
     && notTaken player1 && notTaken player2
     && getLevel newPos board <= 3
     && ((not move) || getLevel newPos board <= getLevel pos board + 1)

-- | Count valid neighbours without materialising the list.
countAdj :: Bool -> Pos -> Players -> Board -> Int
countAdj move pos@(x, y) pls board =
  foldl' (\acc (ox, oy) -> if validAdj move pos pls board (x + ox, y + oy) then acc + 1 else acc)
         0 adjOffsets

-- | The @k@-th (0-based) valid neighbour, in 'adjOffsets' order. Partial only
-- if @k@ is out of range; every caller derives @k@ from 'countAdj'.
nthAdj :: Bool -> Pos -> Players -> Board -> Int -> Pos
nthAdj move pos@(x, y) pls board = go adjOffsets
  where
    go [] _ = error "nthAdj: index out of range"
    go ((ox, oy) : os) k =
      let np = (x + ox, y + oy)
      in if validAdj move pos pls board np
           then if k == 0 then np else go os (k - 1)
           else go os k

isWinningPos :: Board -> Pos -> Bool
isWinningPos board pos = getLevel pos board == 3

isWinningPlayer :: Board -> Player -> Bool
isWinningPlayer board (w1, w2) = isWinningPos board w1 || isWinningPos board w2

switchPlayers :: Players -> Players
switchPlayers (a, b) = (b, a)

-- | True when the player who just moved (now in @snd@) stepped onto level 3,
-- i.e. the player to move at this state has already lost.
moverWon :: GameState -> Bool
moverWon s = isWinningPlayer (getBoard s) (snd (getPlayers s))

-- | Does the player to move have a worker that can step onto a level-3 cell?
-- A winning step never involves a build, so we only test move destinations.
hasWinningMove :: GameState -> Bool
hasWinningMove (GameState _ players board) =
  let (w1, w2) = fst players
  in any (isWinningPos board) (getValidAdjPosnsToMove w1 players board)
     || any (isWinningPos board) (getValidAdjPosnsToMove w2 players board)

-- ----------------------------------------------------------------------------
-- Move enumeration and the single allocation-free sampler
-- ----------------------------------------------------------------------------

-- | A move is the post-move 'Players' plus the build cell (@Nothing@ if the
-- move was itself winning, so no build happens).
type Move = (Players, Maybe Pos)

-- | Canonical, list-based move enumeration (reference semantics).
getValidMoves :: GameState -> [Move]
getValidMoves GameState{getPlayers = players, getBoard = board} = do
  let player = fst players
  (pos, setter) <- zip [fst player, snd player]
                       [\p -> (p, snd player), \p -> (fst player, p)]
  newPos <- getValidAdjPosnsToMove pos players board
  let newPlayers = (setter newPos, snd players)
  buildPos <- if isWinningPos board newPos
                then [Nothing]
                else Just <$> getValidAdjPosnsToBuild newPos newPlayers board
  return (newPlayers, buildPos)

getValidNextStates :: GameState -> [GameState]
getValidNextStates gs = applyMove gs <$> getValidMoves gs

-- | (post-move players, move destination) for each legal worker step, in
-- 'getValidMoves' order. The build choices for entry @i@ are then the valid
-- builds around its destination.
moveEntries :: GameState -> [(Players, Pos)]
moveEntries (GameState _ players board) =
  let (w1, w2) = fst players
      opp      = snd players
  in    [ (((d, w2), opp), d) | d <- getValidAdjPosnsToMove w1 players board ]
     ++ [ (((w1, d), opp), d) | d <- getValidAdjPosnsToMove w2 players board ]

-- | Number of full moves contributed by a single move-entry.
slotsFor :: Board -> (Players, Pos) -> Int
slotsFor board (np, d) = if isWinningPos board d then 1 else countAdj False d np board

-- | Total number of legal moves, == @length (getValidMoves gs)@.
moveCount :: GameState -> Int
moveCount gs@(GameState _ _ board) =
  foldl' (\acc e -> acc + slotsFor board e) 0 (moveEntries gs)

-- | The @k@-th legal move, identical to @getValidMoves gs !! k@, computed
-- without building the full move list. This is the one sampler the engines
-- share; 'randomMove' and the rollouts all go through it.
nthMove :: GameState -> Int -> Move
nthMove gs@(GameState _ _ board) = go (moveEntries gs)
  where
    go [] _ = error "nthMove: index out of range"
    go ((np, d) : rest) k
      | isWinningPos board d = if k == 0 then (np, Nothing) else go rest (k - 1)
      | k < c                = (np, Just (nthAdj False d np board k))
      | otherwise            = go rest (k - c)
      where c = countAdj False d np board

-- | Turn a 'Move' into the resulting 'GameState' (turn+1, players switched,
-- build applied).
applyMove :: GameState -> Move -> GameState
applyMove (GameState turn _ board) (players, mpos) = case mpos of
  Just pos -> GameState (turn + 1) (switchPlayers players) (buildAtPos pos board)
  Nothing  -> GameState (turn + 1) (switchPlayers players) board

-- ----------------------------------------------------------------------------
-- Rollouts
-- ----------------------------------------------------------------------------

-- | A rollout policy: pick the next state, or @Nothing@ if the mover is stuck.
type Policy = GameState -> St Gen (Maybe GameState)

-- | Uniform random move over all legal moves; @Nothing@ when stuck.
randomMove :: Policy
randomMove gs =
  let n = moveCount gs
  in if n == 0
       then pure Nothing
       else do
         k <- stTake (randomR (0, n - 1))
         pure (Just (applyMove gs (nthMove gs k)))

-- | Uniform pick from a non-empty list (caller guarantees non-empty).
uniformPick :: [a] -> St Gen a
uniformPick xs = do
  i <- stTake (randomR (0, length xs - 1))
  pure (xs !! i)

-- | Play a game to a terminal state under @pol@ and report whether the player
-- to move at @start@ is the winner. (Verified equivalent to the original
-- hand-rolled rollout by a parity oracle over thousands of states.)
rollout :: Policy -> GameState -> St Gen Bool
rollout pol = go True
  where
    go ref s
      | moverWon s = pure (not ref)
      | otherwise = do
          mnext <- pol s
          case mnext of
            Nothing -> pure (not ref)
            Just s' -> if moverWon s' then pure ref else go (not ref) s'

rolloutUniform :: GameState -> St Gen Bool
rolloutUniform = rollout randomMove
