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
  , base, enh, enhUcb, enhUnbiased, heavyOnly, solverOnly
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
  , childrenOf
  , mergeRoots
  ) where

import Data.Array (Array, listArray, elems, bounds, (!), (//))
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
  , usePuct :: !Bool   -- prior-directed PUCT + FPU selection (else progressive-bias UCB)
  }

base, enh, enhUcb, enhUnbiased, heavyOnly, solverOnly :: Engine
base        = Engine False False False False -- plain negamax UCT + uniform rollouts
enh         = Engine True  True  True  True  -- solver + heavy + prior-directed PUCT/FPU (default)
enhUcb      = Engine True  True  True  False -- ablation: progressive-bias UCB (pre-PUCT default)
enhUnbiased = Engine True  True  False False -- ablation: shared WU-UCT without bias
heavyOnly   = Engine False True  True  False -- ablation: heavy rollouts + bias
solverOnly  = Engine True  False False False -- ablation: solver only

lookupModern :: String -> Maybe Engine
lookupModern nm = case nm of
  "base"   -> Just base
  "enh"    -> Just enh
  "enh-ucb"  -> Just enhUcb
  "enh-puct" -> Just enh    -- alias: PUCT is now the default enh
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
  | moverWon s = PLoss   -- opponent already stepped onto level 3 (O(1) check)
  | otherwise  = PU      -- a stalemate (no legal moves) is proven when this node
                         -- is expanded: reserveAt/simulate mark empty kids PLoss,
                         -- so we skip the O(#moves) moveCount here (per-child cost).

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
  , mKids   :: !(Maybe (Array Int MNode))
  }

-- | A rollout reserved in the shared tree. The path contains stable child
-- indices from the root to the leaf; child ordering never changes.
data RolloutJob = RolloutJob !GameState ![Int]

-- Children live in a 0-indexed immutable array so selection/backup index in
-- O(1) (the old list rebuilt with splitAt + (before ++ x : after) and indexed
-- with (!!), i.e. O(#children) per node per op). Array order never changes, so
-- RolloutJob's stable child-index paths stay valid across reserve/finish/cancel.
mkKids :: [MNode] -> Array Int MNode
mkKids ks = listArray (0, length ks - 1) ks

kidsList :: Array Int MNode -> [MNode]
kidsList = elems

nullKids :: Array Int MNode -> Bool
nullKids ks = case bounds ks of (lo, hi) -> hi < lo

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
  | Just ks <- mKids n, nullKids ks = pure (0, bump 0 n)  -- stalemate: no moves -> lost
  | mProven n == PWin   = pure (1, bump 1 n)
  | mProven n == PLoss  = pure (0, bump 0 n)
simulate eng n@(MNode s r v _ _ _ Nothing) =
  let nexts = getValidNextStates s
  in if null nexts
       then pure (0, n { mProven = if useSolver eng then PLoss else PU
                       , mKids = Just (mkKids []), mVisits = v + 1, mReward = r })
       else do
         won <- rollout (policyFor eng) s
         let v0   = if won then 1 else 0
             kids = map (mkChild eng) nexts
             pv   = if useSolver eng then aggregatePV kids else PU
         pure (provenOr pv v0, n { mKids = Just (mkKids kids), mVisits = v + 1, mReward = r + v0, mProven = pv })
simulate eng n@(MNode _ r v pending _ _ (Just kids)) = do
  i <- selectChild eng v pending kids
  let selected = kids ! i
  (vChild, kid') <- simulate eng selected
  let kids' = kids // [(i, kid')]
      vSelf = 1 - vChild
      pv
        | not (useSolver eng) = PU
        | mProven kid' == PLoss = PWin
        | mProven kid' == PWin && all ((== PWin) . mProven) (kidsList kids') = PLoss
        | otherwise = PU
  pure (provenOr pv vSelf, n { mKids = Just kids', mVisits = v + 1, mReward = r + vSelf, mProven = pv })

ucbC :: Double
ucbC = sqrt 2

-- | UCT child selection. Reward is stored from the child mover's perspective,
-- so exploitation for THIS node is @1 - reward/visits@ (negamax).
selectChild :: Engine -> Int -> Int -> Array Int MNode -> St Gen Int
selectChild eng parentVisits parentPending kids
  | usePuct eng = puctSelect eng parentVisits parentPending kids
  | otherwise   = ucbSelect eng parentVisits parentPending kids

-- Prior-directed PUCT with first-play urgency: score each child q + U, where
-- U = cpuct * P(c) * sqrt(parent N+O) / (1 + child N+O), P is a softmax over the
-- static positional prior, and q is the negamax win-rate (FPU for unvisited
-- children). Unlike 'ucbSelect' it does NOT force one visit of every child
-- first, so search concentrates on high-prior moves. Exploitation stays N-only;
-- exploration and the parent count use N+O (WU-UCT); proven wins stay masked.
puctSelect :: Engine -> Int -> Int -> Array Int MNode -> St Gen Int
puctSelect eng parentVisits parentPending kids =
  let pairs = zip [0 ..] (kidsList kids)
      maxPrior = fromIntegral (maximum (map (mPrior . snd) pairs)) :: Double
      weight c = exp ((fromIntegral (mPrior c) - maxPrior) / puctPriorTemp)
      z = sum [ weight c | (_, c) <- pairs ]
      sqrtParent = sqrt (fromIntegral (max 1 (parentVisits + parentPending))) :: Double
      score c
        | useSolver eng && mProven c == PWin = -1e18   -- never steer into the opponent's proven win
        | otherwise =
            let observed = mVisits c
                q | observed == 0 = puctFpu
                  | otherwise = 1 - fromIntegral (mReward c) / fromIntegral observed
                p = weight c / z
                u = puctC * p * sqrtParent / fromIntegral (1 + observed + mPending c)
            in q + u
      scored = [ (i, score c) | (i, c) <- pairs ]
      best = maximum (map snd scored)
      bestIdx = [ i | (i, s) <- scored, s == best ]
  in case bestIdx of
       [only] -> pure only
       _      -> uniformPick bestIdx

-- PUCT tuning constants (starting points; tune by self-play).
puctC, puctFpu, puctPriorTemp :: Double
puctC = 1.5          -- exploration weight (cf. ucbC = sqrt 2 for ucbSelect)
puctFpu = 0.45       -- first-play urgency: assumed win-rate of an unvisited child
puctPriorTemp = 120  -- softmax temperature over the static positional prior

ucbSelect :: Engine -> Int -> Int -> Array Int MNode -> St Gen Int
ucbSelect eng parentVisits parentPending kids =
  let pairs = zip [0 ..] (kidsList kids)   -- (index, child); no (!!) re-indexing
      unseen =
        [ (i, c)
        | (i, c) <- pairs
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
      scored = [ (i, pr c) | (i, c) <- pairs ]
      best = maximum (map snd scored)
      -- Break exact-tie UCB scores uniformly at random instead of always taking
      -- the last child: the old `score >= bestScore` fold biased selection
      -- toward worker-2 steps and later build cells (moveEntries order).
      bestSeen = [ i | (i, s) <- scored, s == best ]
      bestUnseen
        | not (useBias eng) = map fst unseen
        | otherwise =
            let bestPrior = maximum (map (mPrior . snd) unseen)
            in [ i | (i, c) <- unseen, mPrior c == bestPrior ]
  in if not (null unseen)
       then uniformPick bestUnseen
       else case bestSeen of
              [only] -> pure only          -- unique argmax: no RNG draw (as before)
              _      -> uniformPick bestSeen

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
          n' = n { mPending = mPending + 1, mProven = pv, mKids = Just (mkKids kids) }
      in pure (Just (RolloutJob mState []), n')
    reserveAt n@MNode{mState, mVisits, mPending, mKids = Just kids}
      | nullKids kids =
          pure (Just (RolloutJob mState []), n { mPending = mPending + 1 })
      | otherwise = do
          i <- selectChild eng mVisits mPending kids
          let selected = kids ! i
          (job, selected') <- reserveAt selected
          let kids' = kids // [(i, selected')]
              pv = if useSolver eng then aggregatePV (kidsList kids') else PU
              n' = n { mKids = Just kids', mProven = pv }
          pure $ case job of
            Nothing -> (Nothing, n')
            Just (RolloutJob state path) ->
              (Just (RolloutJob state (i : path)), n' { mPending = mPending + 1 })

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
      case mKids n of
        Just kids ->
          let (childValue, selected') = finishAt rest leafValue (kids ! i)
              kids' = kids // [(i, selected')]
              selfValue = 1 - childValue
              pv = if useSolver eng then aggregatePV (kidsList kids') else PU
              n' = n
                { mPending = mPending n - 1
                , mVisits = mVisits n + 1
                , mReward = mReward n + selfValue
                , mProven = pv
                , mKids = Just kids'
                }
          in (provenOr pv selfValue, n')
        Nothing -> error "finishRollout: reserved child index out of range"

-- | Remove a reservation whose worker failed, without inventing a result.
cancelRollout :: RolloutJob -> MNode -> MNode
cancelRollout (RolloutJob _ path) = cancelAt path
  where
    cancelAt [] n = n { mPending = mPending n - 1 }
    cancelAt (i : rest) n =
      case mKids n of
        Just kids ->
          n
            { mPending = mPending n - 1
            , mKids = Just (kids // [(i, cancelAt rest (kids ! i))])
            }
        Nothing -> error "cancelRollout: reserved child index out of range"

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

-- | Merge several independently-searched roots of the SAME position into one
-- root for final move choice (root parallelization). Every tree expands the
-- root's children from getValidNextStates in the same order, so child index j
-- names the same move in every tree; visits and rewards are summed and exact
-- solver proofs are OR-combined. Merged children carry no subtrees, since root
-- choice reads only their visits/proofs/state.
mergeRoots :: [MNode] -> MNode
mergeRoots []             = error "mergeRoots: no roots"
mergeRoots [r]            = r
mergeRoots roots@(r0 : _) =
  let columns = map childrenOf roots
      width = maximum (0 : map length columns)
  in if width == 0
       then r0
       else r0
         { mVisits  = sum (map mVisits roots)
         , mReward  = sum (map mReward roots)
         , mPending = 0
         , mProven  = foldr combineProof PU (map mProven roots)
         , mKids    = Just (mkKids [ mergeColumn j columns | j <- [0 .. width - 1] ])
         }
  where
    mergeColumn j cols =
      case [ ks !! j | ks <- cols, length ks > j ] of
        []          -> error "mergeRoots: empty child column"
        cs@(c0 : _) -> c0
          { mVisits  = sum (map mVisits cs)
          , mReward  = sum (map mReward cs)
          , mPending = 0
          , mProven  = foldr combineProof PU (map mProven cs)
          , mKids    = Nothing
          }

-- Combine two EXACT proofs of the same position from different trees. Proofs
-- are consistent (a position cannot be both a proven win and a proven loss), so
-- any non-PU proof dominates.
combineProof :: Proven -> Proven -> Proven
combineProof PWin  _     = PWin
combineProof _     PWin  = PWin
combineProof PLoss _     = PLoss
combineProof _     PLoss = PLoss
combineProof PU    PU    = PU

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
  let cs = maybe [] kidsList (mKids t)
  in mVisits t + mPending t + sum (map mVisits cs)
       + length (filter ((/= PU) . mProven) cs)

childrenOf :: MNode -> [MNode]
childrenOf = maybe [] kidsList . mKids
