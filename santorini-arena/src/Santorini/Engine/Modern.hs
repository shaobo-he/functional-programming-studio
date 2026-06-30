{-# LANGUAGE NamedFieldPuns #-}

-- | The \"modern\" engine: a recursive, negamax-correct MCTS with an optional
-- exact MCTS-Solver and optional heavy (rule-guided) rollouts, plus a pure
-- root-parallel ensemble. This is the rewritten engine, with the fixes from the
-- code review applied:
--
--   * 'simulate' is total ('-Wincomplete-patterns' clean).
--   * 'chooseMoveRootPar' guards an empty root (no more @maximumBy: empty@).
--   * the dead @PLoss@ branch in 'selectChild' is removed.
--   * the move sampler / rollouts live once in "Santorini.Core" (no duplication).
module Santorini.Engine.Modern
  ( Engine(..)
  , base, enh, heavyOnly, solverOnly
  , lookupModern
  , chooseMove
    -- * Anytime / resumable interface (for the time-bounded parallel driver)
  , MNode
  , mkRoot
  , advance
  , nodeVisits
  , rootSettled
  , forceTreeStats
  , combineRoots
  ) where

import Data.Function (on)
import Data.List (maximumBy)

import Santorini.Core

-- ----------------------------------------------------------------------------
-- Engine configuration
-- ----------------------------------------------------------------------------

data Engine = Engine { useSolver :: !Bool, useHeavy :: !Bool }

base, enh, heavyOnly, solverOnly :: Engine
base       = Engine False False   -- plain negamax UCT + uniform rollouts
enh        = Engine True  True    -- MCTS-Solver + 2-rule heavy rollouts
heavyOnly  = Engine False True    -- ablation: heavy rollouts only
solverOnly = Engine True  False   -- ablation: solver only

lookupModern :: String -> Maybe Engine
lookupModern nm = case nm of
  "base"   -> Just base
  "enh"    -> Just enh
  "heavy"  -> Just heavyOnly
  "solver" -> Just solverOnly
  _        -> Nothing

-- ----------------------------------------------------------------------------
-- Exact proof layer (MCTS-Solver). Perspective: the player TO MOVE at the node.
-- ----------------------------------------------------------------------------

data Proven = PU | PWin | PLoss deriving (Eq, Show)

terminalPV :: GameState -> Proven
terminalPV s = if moverWon s then PLoss else PU

-- | I (the mover) win if any of my moves hands the opponent a proven loss;
-- I lose if every move I have leaves the opponent proven winning.
aggregatePV :: [MNode] -> Proven
aggregatePV kids
  | any ((== PLoss) . mProven) kids = PWin
  | all ((== PWin)  . mProven) kids = PLoss
  | otherwise                       = PU

-- | A proof dominates the statistical estimate.
provenOr :: Proven -> Int -> Int
provenOr PWin  _ = 1
provenOr PLoss _ = 0
provenOr PU    x = x

-- ----------------------------------------------------------------------------
-- Search tree node
-- ----------------------------------------------------------------------------

data MNode = MNode
  { mState  :: !GameState
  , mReward :: {-# UNPACK #-} !Int
  , mVisits :: {-# UNPACK #-} !Int
  , mProven :: !Proven
  , mKids   :: !(Maybe [MNode])
  }

-- | Rollout policy for an engine: rule-guided when heavy, else uniform.
policyFor :: Engine -> Policy
policyFor eng = if useHeavy eng then heavyPolicy else randomMove

-- | Heavy rollout policy: (1) take a winning step if one exists; otherwise
-- (2) avoid handing the opponent an immediate win.
heavyPolicy :: Policy
heavyPolicy s@(GameState turn players board) =
  let (w1, w2) = fst players
      winNexts =
        [ GameState (turn + 1) (switchPlayers (mk d, snd players)) board
        | (w, mk) <- [(w1, \d -> (d, w2)), (w2, \d -> (w1, d))]
        , d <- getValidAdjPosnsToMove w players board
        , isWinningPos board d
        ]
  in if not (null winNexts)
       then Just <$> uniformPick winNexts
       else
         let nexts = getValidNextStates s
         in if null nexts
              then pure Nothing
              else let safe = filter (not . hasWinningMove) nexts
                   in Just <$> uniformPick (if null safe then nexts else safe)

bump :: Int -> MNode -> MNode
bump x n = n { mVisits = mVisits n + 1, mReward = mReward n + x }

-- | One MCTS iteration over a node, returning the value FROM THE NODE MOVER'S
-- perspective and the updated node. The negamax negation is the @1 - vChild@.
simulate :: Engine -> MNode -> St Gen (Int, MNode)
simulate _ n
  | moverWon (mState n) = pure (0, bump 0 n)   -- opponent already stepped onto level 3
  | Just [] <- mKids n  = pure (0, bump 0 n)   -- stalemate: mover has no moves -> lost
  | mProven n == PWin   = pure (1, bump 1 n)
  | mProven n == PLoss  = pure (0, bump 0 n)
simulate eng n@(MNode s r v _ Nothing) =
  let nexts = getValidNextStates s
  in if null nexts
       then pure (0, n { mProven = if useSolver eng then PLoss else PU
                       , mKids = Just [], mVisits = v + 1, mReward = r })
       else do
         won <- rollout (policyFor eng) s
         let v0   = if won then 1 else 0
             kids = [ MNode c 0 0 (if useSolver eng then terminalPV c else PU) Nothing | c <- nexts ]
             pv   = if useSolver eng then aggregatePV kids else PU
         pure (provenOr pv v0, n { mKids = Just kids, mVisits = v + 1, mReward = r + v0, mProven = pv })
simulate eng n@(MNode _ r v _ (Just kids)) = do
  let i = selectChild eng v kids
  (vChild, kid') <- simulate eng (kids !! i)
  let kids' = setAt i kid' kids
      vSelf = 1 - vChild
      pv    = if useSolver eng then aggregatePV kids' else PU
  pure (provenOr pv vSelf, n { mKids = Just kids', mVisits = v + 1, mReward = r + vSelf, mProven = pv })

ucbC :: Double
ucbC = sqrt 2

-- | UCT child selection. Reward is stored from the child mover's perspective,
-- so exploitation for THIS node is @1 - reward/visits@ (negamax).
selectChild :: Engine -> Int -> [MNode] -> Int
selectChild eng parentVisits kids =
  let lpv = log (fromIntegral (max 1 parentVisits)) :: Double
      pr c | useSolver eng && mProven c == PWin = -1e18   -- never steer into the opponent's proven win
           | mVisits c == 0                     = 2e18    -- expand unseen children first
           | otherwise = let vis = fromIntegral (mVisits c)
                         in (1 - fromIntegral (mReward c) / vis) + ucbC * sqrt (lpv / vis)
  in argmaxIndex (map pr kids)

argmaxIndex :: [Double] -> Int
argmaxIndex xs = snd $ maximumBy (compare `on` fst) (zip xs [0 ..])

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = case splitAt i xs of
  (l, _ : r) -> l ++ x : r
  (l, [])    -> l   -- out of range: unreachable (i always a valid child index)

runSims :: Engine -> Int -> MNode -> St Gen MNode
runSims _   0 n = pure n
runSims eng b n
  | useSolver eng && mProven n /= PU = pure n   -- root already solved
  | otherwise = simulate eng n >>= \(_, n') -> runSims eng (b - 1) n'

-- | Run @budget@ simulations from @s@ and return the chosen successor state.
chooseMove :: Engine -> Int -> GameState -> St Gen GameState
chooseMove eng budget s = do
  root <- runSims eng budget (MNode s 0 0 (if useSolver eng then terminalPV s else PU) Nothing)
  let kids = maybe [] id (mKids root)
  if null kids
    then pure s
    else do
      let provenWin = filter ((== PLoss) . mProven) kids   -- a PLoss child is a proven win for us
          nonLoss   = filter ((/= PWin)  . mProven) kids
          pool | useSolver eng && not (null provenWin) = provenWin
               | not (null nonLoss)                    = nonLoss
               | otherwise                             = kids
      pure (mState (maximumBy (compare `on` mVisits) pool))

-- ----------------------------------------------------------------------------
-- Anytime / resumable interface for the time-bounded parallel driver.
-- ----------------------------------------------------------------------------

-- | A fresh root node for game state @s@.
mkRoot :: Engine -> GameState -> MNode
mkRoot eng s = MNode s 0 0 (if useSolver eng then terminalPV s else PU) Nothing

-- | Advance a node by @n@ simulations (resumable: feed the result back in).
advance :: Engine -> Int -> MNode -> St Gen MNode
advance = runSims

-- | Visit count at a node.
nodeVisits :: MNode -> Int
nodeVisits = mVisits

-- | True once the solver has settled the root, so the search can stop early.
rootSettled :: Engine -> MNode -> Bool
rootSettled eng n = useSolver eng && mProven n /= PU

-- | Forcing this realises every root-child statistic the combine needs, so a
-- worker thread can pull the work off the lazy heap before it returns its tree.
forceTreeStats :: MNode -> Int
forceTreeStats t =
  let cs = maybe [] id (mKids t)
  in sum (map mVisits cs) + length (filter ((== PLoss) . mProven) cs)

-- | Combine N independently-grown roots (same state, different seeds) into the
-- chosen successor: sum each child's visits across trees and take the
-- max-visited child, but jump on any child a solver tree proved winning. Falls
-- back to @s@ if no tree expanded the root (terminal).
combineRoots :: Engine -> [MNode] -> GameState -> GameState
combineRoots eng trees s =
  case [ t | t <- trees, not (null (childrenOf t)) ] of
    []                -> case getValidNextStates s of (n : _) -> n; [] -> s  -- still emit a legal move
    withKids@(t0 : _) ->
      let kids0        = childrenOf t0
          nc           = length kids0
          visAt t i    = let ks = childrenOf t in if i < length ks then mVisits (ks !! i) else 0
          provenAt t i = let ks = childrenOf t in i < length ks && mProven (ks !! i) == PLoss
          summed       = [ sum [ visAt t i | t <- withKids ] | i <- [0 .. nc - 1] ]
          provenW      = [ i | i <- [0 .. nc - 1], any (\t -> provenAt t i) withKids ]
          idx | useSolver eng && not (null provenW) = head provenW
              | otherwise                           = argmaxIndex (map fromIntegral summed)
      in mState (kids0 !! idx)
  where
    childrenOf t = maybe [] id (mKids t)
