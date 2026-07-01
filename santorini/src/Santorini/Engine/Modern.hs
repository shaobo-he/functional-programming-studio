{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The \"modern\" engine: a recursive, negamax-correct MCTS with an optional
-- exact MCTS-Solver and optional heavy (rule-guided) rollouts. The pure tree API
-- supports a single logical tree with parallel rollout evaluation. This is the
-- rewritten engine, with the fixes from the
-- code review applied:
--
--   * 'simulate' is total ('-Wincomplete-patterns' clean).
--   * root selection guards an empty root (no more @maximumBy: empty@).
--   * the dead @PLoss@ branch in 'selectChild' is removed.
--   * the move sampler / rollouts live once in "Santorini.Core" (no duplication).
module Santorini.Engine.Modern
  ( Engine(..)
  , base, enh, enhUnbiased, heavyOnly, solverOnly
  , lookupModern
  , chooseMove
    -- * Anytime / resumable interface (for the time-bounded parallel driver)
  , MNode
  , mkRoot
  , advance
  , nodeState
  , nodeVisits
  , nodePending
  , rootSettled
  , forceTreeStats
  , RolloutJob
  , reserveRollout
  , runRolloutJob
  , finishRollout
  , cancelRollout
  , chooseRootChild
  , reuseRoot
  ) where

import Data.Function (on)
import Data.List (foldl', maximumBy)

import Santorini.Core

-- ----------------------------------------------------------------------------
-- Engine configuration
-- ----------------------------------------------------------------------------

data Engine = Engine
  { useSolver :: !Bool
  , useHeavy :: !Bool
  , useBias :: !Bool
  }

base, enh, enhUnbiased, heavyOnly, solverOnly :: Engine
base        = Engine False False False -- plain negamax UCT + uniform rollouts
enh         = Engine True  True  True  -- solver + heavy rollouts + progressive bias
enhUnbiased = Engine True  True  False -- ablation: shared WU-UCT without bias
heavyOnly   = Engine False True  True  -- ablation: heavy rollouts + bias
solverOnly  = Engine True  False False -- ablation: solver only

lookupModern :: String -> Maybe Engine
lookupModern nm = case nm of
  "base"   -> Just base
  "enh"    -> Just enh
  "enh-unbiased" -> Just enhUnbiased
  "heavy"  -> Just heavyOnly
  "solver" -> Just solverOnly
  _        -> Nothing

-- ----------------------------------------------------------------------------
-- Exact proof layer (MCTS-Solver). Perspective: the player TO MOVE at the node.
-- ----------------------------------------------------------------------------

data Proven = PU | PWin | PLoss deriving (Eq, Show)

terminalPV :: GameState -> Proven
terminalPV s
  | moverWon s      = PLoss
  | moveCount s == 0 = PLoss
  | otherwise       = PU

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
  , mPending :: {-# UNPACK #-} !Int
  , mPrior :: {-# UNPACK #-} !Int
  , mProven :: !Proven
  , mKids   :: !(Maybe [MNode])
  }

-- | A rollout reserved in the shared tree. The path contains stable child
-- indices from the root to the leaf; child ordering never changes.
data RolloutJob = RolloutJob !GameState ![Int]

-- | Rollout policy for an engine: rule-guided when heavy, else uniform.
policyFor :: Engine -> Policy
policyFor eng = if useHeavy eng then heavyPolicy else randomMove

-- | Heavy rollout policy: take a winning step, avoid handing the opponent an
-- immediate win, then prefer height, threats, and central worker positions.
-- Random tie-breaking keeps rollouts diverse without treating obviously weak
-- and strong safe moves as equivalent.
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
                       candidates = if null safe then nexts else safe
                       best = bestRolloutMoves candidates
                   in Just <$> uniformPick best

bestRolloutMoves :: [GameState] -> [GameState]
bestRolloutMoves [] = []
bestRolloutMoves (first : rest) = snd (foldl' keep (rolloutScore first, [first]) rest)
  where
    keep (!bestScore, best) candidate =
      let score = rolloutScore candidate
      in case compare score bestScore of
           GT -> (score, [candidate])
           EQ -> (bestScore, candidate : best)
           LT -> (bestScore, best)

-- The successor stores the opponent as the player to move and the player who
-- made the candidate move second. Score from that second player's perspective.
rolloutScore :: GameState -> Int
rolloutScore next =
  let (opponent, us) = getPlayers next
      board = getBoard next
      heights (a, b) = (getLevel a board, getLevel b board)
      (ha, hb) = heights us
      (oa, ob) = heights opponent
      centrality (r, c) = 2 - max (abs (r - 3)) (abs (c - 3))
      centre = centrality (fst us) + centrality (snd us)
      ourTurn = next { getPlayers = (us, opponent) }
      threat = if hasWinningMove ourTurn then 100 else 0
  in threat + 20 * (ha + hb) + 8 * max ha hb - 12 * (oa + ob) + 2 * centre

-- Static score from the parent mover's perspective. A terminal winning step
-- must sort ahead of every positional preference.
childPrior :: GameState -> Int
childPrior state
  | moverWon state = 10000
  | otherwise = rolloutScore state

mkChild :: Engine -> GameState -> MNode
mkChild eng state =
  MNode state 0 0 0 (childPrior state)
    (if useSolver eng then terminalPV state else PU) Nothing

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
simulate eng n@(MNode s r v _ _ _ Nothing) =
  let nexts = getValidNextStates s
  in if null nexts
       then pure (0, n { mProven = if useSolver eng then PLoss else PU
                       , mKids = Just [], mVisits = v + 1, mReward = r })
       else do
         won <- rollout (policyFor eng) s
         let v0   = if won then 1 else 0
             kids = map (mkChild eng) nexts
             pv   = if useSolver eng then aggregatePV kids else PU
         pure (provenOr pv v0, n { mKids = Just kids, mVisits = v + 1, mReward = r + v0, mProven = pv })
simulate eng n@(MNode _ r v pending _ _ (Just kids)) = do
  i <- selectChild eng v pending kids
  case splitAt i kids of
    (before, selected : after) -> do
      (vChild, kid') <- simulate eng selected
      let kids' = before ++ kid' : after
          vSelf = 1 - vChild
          pv
            | not (useSolver eng) = PU
            | mProven kid' == PLoss = PWin
            | mProven kid' == PWin && all ((== PWin) . mProven) kids' = PLoss
            | otherwise = PU
      pure (provenOr pv vSelf, n { mKids = Just kids', mVisits = v + 1, mReward = r + vSelf, mProven = pv })
    _ -> error "simulate: selected child index out of range"

ucbC :: Double
ucbC = sqrt 2

-- | UCT child selection. Reward is stored from the child mover's perspective,
-- so exploitation for THIS node is @1 - reward/visits@ (negamax).
selectChild :: Engine -> Int -> Int -> [MNode] -> St Gen Int
selectChild eng parentVisits parentPending kids =
  let unseen =
        [ i
        | (i, c) <- zip [0 ..] kids
        , mVisits c + mPending c == 0
        , not (useSolver eng && mProven c == PWin)
        ]
      lpv = log (fromIntegral (max 1 (parentVisits + parentPending))) :: Double
      -- Progressive bias affects early allocation, then decays as direct
      -- evidence accumulates. The bounded tanh keeps tactics subordinate to Q.
      bias c
        | not (useBias eng) = 0
        | otherwise =
            let h = tanh (fromIntegral (mPrior c) / 80) :: Double
            in 0.5 * h / fromIntegral (1 + mVisits c + mPending c)
      pr c | useSolver eng && mProven c == PWin = -1e18   -- never steer into the opponent's proven win
           | otherwise =
               let observed = mVisits c
                   total = max 1 (observed + mPending c)
                   exploitation
                     | observed == 0 = 0.5
                     | otherwise = 1 - fromIntegral (mReward c) / fromIntegral observed
               in exploitation + ucbC * sqrt (lpv / fromIntegral total) + bias c
      go !_ [] !bestIndex !_ = bestIndex
      go !i (c : cs) !bestIndex !bestScore =
        let score = pr c
        in if score >= bestScore
             then go (i + 1) cs i score
             else go (i + 1) cs bestIndex bestScore
      bestUnseen
        | not (useBias eng) = unseen
        | otherwise =
            let bestPrior = maximum (map (mPrior . (kids !!)) unseen)
            in filter ((== bestPrior) . mPrior . (kids !!)) unseen
  in if not (null unseen)
       then uniformPick bestUnseen
       else case kids of
              [] -> error "selectChild: empty children"
              c : cs -> pure (go 1 cs 0 (pr c))

runSims :: Engine -> Int -> MNode -> St Gen MNode
runSims _   0 n = pure n
runSims eng b n
  | useSolver eng && mProven n /= PU = pure n   -- root already solved
  | otherwise = simulate eng n >>= \(_, n') -> runSims eng (b - 1) n'

-- | Run @budget@ simulations from @s@ and return the chosen successor state.
chooseMove :: Engine -> Int -> GameState -> St Gen GameState
chooseMove eng budget s = do
  root <- runSims eng budget (mkRoot eng s)
  pure (maybe s mState (chooseRootChild eng root))

-- ----------------------------------------------------------------------------
-- Anytime / resumable interface for the time-bounded parallel driver.
-- ----------------------------------------------------------------------------

-- | A fresh root node for game state @s@.
mkRoot :: Engine -> GameState -> MNode
mkRoot eng s = MNode s 0 0 0 0 (if useSolver eng then terminalPV s else PU) Nothing

-- | Advance a node by @n@ simulations (resumable: feed the result back in).
advance :: Engine -> Int -> MNode -> St Gen MNode
advance = runSims

-- | Reserve one simulation path in the shared tree. Pending counts are added
-- immediately, so later reservations account for work already in flight.
reserveRollout :: Engine -> MNode -> St Gen (Maybe RolloutJob, MNode)
reserveRollout eng root
  | useSolver eng && mProven root /= PU = pure (Nothing, root)
  | otherwise = reserveAt root
  where
    reserveAt n@MNode{mState, mPending, mKids = Nothing} =
      let nexts = getValidNextStates mState
          kids = map (mkChild eng) nexts
          pv
            | not (useSolver eng) = PU
            | null kids = PLoss
            | otherwise = aggregatePV kids
          n' = n { mPending = mPending + 1, mProven = pv, mKids = Just kids }
      in pure (Just (RolloutJob mState []), n')
    reserveAt n@MNode{mState, mPending, mKids = Just []} =
      pure (Just (RolloutJob mState []), n { mPending = mPending + 1 })
    reserveAt n@MNode{mVisits, mPending, mKids = Just kids} = do
      i <- selectChild eng mVisits mPending kids
      case splitAt i kids of
        (before, selected : after) -> do
          (job, selected') <- reserveAt selected
          let kids' = before ++ selected' : after
              pv = if useSolver eng then aggregatePV kids' else PU
              n' = n { mKids = Just kids', mProven = pv }
          pure $ case job of
            Nothing -> (Nothing, n')
            Just (RolloutJob state path) ->
              (Just (RolloutJob state (i : path)), n' { mPending = mPending + 1 })
        _ -> error "reserveRollout: selected child index out of range"

-- | Evaluate a reserved leaf. The result is from the leaf mover's perspective.
runRolloutJob :: Engine -> RolloutJob -> St Gen Int
runRolloutJob eng (RolloutJob state _) = do
  won <- rollout (policyFor eng) state
  pure (if won then 1 else 0)

-- | Incorporate a completed rollout and remove its pending reservation.
finishRollout :: Engine -> RolloutJob -> Int -> MNode -> MNode
finishRollout eng (RolloutJob _ path) value root = snd (finishAt path value root)
  where
    finishAt [] leafValue n =
      let n' = n
            { mPending = mPending n - 1
            , mVisits = mVisits n + 1
            , mReward = mReward n + leafValue
            }
      in (provenOr (mProven n') leafValue, n')
    finishAt (i : rest) leafValue n =
      case splitAt i (childrenOf n) of
        (before, selected : after) ->
          let (childValue, selected') = finishAt rest leafValue selected
              kids' = before ++ selected' : after
              selfValue = 1 - childValue
              pv = if useSolver eng then aggregatePV kids' else PU
              n' = n
                { mPending = mPending n - 1
                , mVisits = mVisits n + 1
                , mReward = mReward n + selfValue
                , mProven = pv
                , mKids = Just kids'
                }
          in (provenOr pv selfValue, n')
        _ -> error "finishRollout: reserved child index out of range"

-- | Remove a reservation whose worker failed, without inventing a result.
cancelRollout :: RolloutJob -> MNode -> MNode
cancelRollout (RolloutJob _ path) = cancelAt path
  where
    cancelAt [] n = n { mPending = mPending n - 1 }
    cancelAt (i : rest) n =
      case splitAt i (childrenOf n) of
        (before, selected : after) ->
          n
            { mPending = mPending n - 1
            , mKids = Just (before ++ cancelAt rest selected : after)
            }
        _ -> error "cancelRollout: reserved child index out of range"

-- | Select the robust child while respecting exact proofs and the immediate
-- tactical guard. Shared-tree search returns this node for reuse next turn.
chooseRootChild :: Engine -> MNode -> Maybe MNode
chooseRootChild eng root =
  case childrenOf root of
    [] -> Nothing
    kids ->
      let provenWin = filter ((== PLoss) . mProven) kids
          nonLoss = filter ((/= PWin) . mProven) kids
          candidates
            | useSolver eng && not (null provenWin) = provenWin
            | useSolver eng && not (null nonLoss) = nonLoss
            | otherwise = kids
          safe = filter (not . hasWinningMove . mState) candidates
          pool
            | useSolver eng && not (null provenWin) = provenWin
            | not (null safe) = safe
            | otherwise = candidates
      in Just (maximumBy (compare `on` mVisits) pool)

-- | Reuse an exact cached root, or its child after the opponent's reply.
reuseRoot :: Engine -> GameState -> Maybe MNode -> MNode
reuseRoot eng state cached =
  case cached of
    Just root
      | mState root == state -> root
      | otherwise -> case filter ((== state) . mState) (childrenOf root) of
          child : _ -> child
          [] -> mkRoot eng state
    Nothing -> mkRoot eng state

-- | Position represented by a node.
nodeState :: MNode -> GameState
nodeState = mState

-- | Visit count at a node.
nodeVisits :: MNode -> Int
nodeVisits = mVisits

-- | Number of dispatched rollouts whose results have not reached this node.
nodePending :: MNode -> Int
nodePending = mPending

-- | True once the solver has settled the root, so the search can stop early.
rootSettled :: Engine -> MNode -> Bool
rootSettled eng n = useSolver eng && mProven n /= PU

-- | Force the strict statistics touched by coordinator updates before the next
-- rollout is dispatched or the tree is returned to the protocol loop.
forceTreeStats :: MNode -> Int
forceTreeStats t =
  let cs = maybe [] id (mKids t)
  in mVisits t + mPending t + sum (map mVisits cs)
       + length (filter ((/= PU) . mProven) cs)

childrenOf :: MNode -> [MNode]
childrenOf = maybe [] id . mKids
