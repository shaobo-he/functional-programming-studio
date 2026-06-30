module MCTS (
  mcts,
  -- anytime / resumable API for the time-bounded parallel driver
  mkRootTree,
  stepRootN,
  rootChildVisits,
  rootChildStates,
  rootForce,
) where

import Data.Tree
import Data.List (maximumBy)
import Data.Function (on)
import Control.Monad.State.Lazy
import System.Random
import Santorini

-- Strict counters: without this the lazy (+v)/(+1) build thunk chains across a
-- whole move's search, blowing residency to hundreds of MB on endgame boards
-- (and OOM-forfeiting under a heap limit). Matches the modern engine's node.
data SearchState = SearchState { getGameState :: !GameState
                               , getReward :: {-# UNPACK #-} !Int
                               , getVisits :: {-# UNPACK #-} !Int
                               } deriving (Show)

type SearchTree = Tree SearchState

data STCrumb = STCrumb { getSearchTree :: SearchTree
                       , getLT :: [SearchTree]
                       , getRT :: [SearchTree]
                       } deriving (Show)

type STZipper = (SearchTree, [STCrumb])

-- | Add @v@ to the immediate parent and pop one crumb.
backProp' :: Int -> STZipper -> STZipper
backProp' _ stz@(_, []) = stz -- reached root
backProp' v (st, fcb:rcbs) =
    let oldState = rootLabel . getSearchTree $ fcb
        newState = SearchState
          (getGameState oldState)
          ((getReward oldState) + v)
          ((getVisits oldState) + 1) in
            ((Node newState $ (getLT fcb)++[st]++(getRT fcb)), rcbs)

-- | Negamax backup: the value is negated (1 - v) at every level, because each
-- node stores its reward from the perspective of the player to move there.
backProp :: Int -> STZipper -> STZipper
backProp _ stz@(_, []) = stz
backProp v stz = backProp (1 - v) (backProp' v stz)

isVisited :: SearchState -> Bool
isVisited (SearchState _ _ visits) = visits > 0

isFullyExpanded :: SearchTree -> Bool
isFullyExpanded (Node _ []) = False
isFullyExpanded (Node _ children)  = all (isVisited . rootLabel) children

isUnexpanded :: SearchTree -> Bool
isUnexpanded (Node _ children) = null children

mkFreshSearchState :: GameState -> SearchState
mkFreshSearchState gs = SearchState gs 0 0

mkFreshSTNode :: GameState -> SearchTree
mkFreshSTNode gs = Node (mkFreshSearchState gs) []

-- | UCT scores for a node's children. Exploitation is @1 - reward/visits@:
-- a child stores its win-rate from the child-mover's (opponent's) perspective,
-- so the parent maximizes its own rate, which is one minus the child's.
getUCT :: SearchTree -> [Double]
getUCT (Node ss children) = getUCTOfChild <$> children where
  rootVisits = fromIntegral . getVisits $ ss
  getUCTOfChild child = let css = rootLabel child
                            reward = fromIntegral . getReward $ css
                            visits = fromIntegral . getVisits $ css in
                              (1 - (reward/visits)) +
                              (sqrt $ (2*(log rootVisits))/visits)

pickCandidate :: STZipper -> STZipper
pickCandidate stz@(st, cbs)
  -- unexpanded node -> expands it and then pick
  | isUnexpanded st =
    let cgs = getGameState . rootLabel $ st in
      if isWinningPlayer (getBoard cgs) (snd (getPlayers cgs))
        then stz
        else let moves = getValidNextStates cgs in
          if null moves
            then stz
            else pickCandidate $ ((Node (rootLabel st) (mkFreshSTNode <$> moves)), cbs)
  -- fully expanded node -> select best child
  | isFullyExpanded st = pickCandidate getChildByBestUCT
  -- get the first unvisited child
  | otherwise = let (child, l, r) = getUnvisitedChild in
      (child, (STCrumb st l r):cbs)
    where
      getChildByBestUCT = let children = subForest st
                              ucts = getUCT st
                              bestId = snd $ maximumBy (compare `on` fst) $ zip ucts [0..] in
                              (children !! bestId,
                                (STCrumb st (take bestId children) (drop (bestId+1) children)):cbs)
      getUnvisitedChild = let children = subForest st
                              (l, rest) = span (isVisited . rootLabel) children in
                            case rest of
                              (f:r) -> (f, l, r)
                              -- unreachable: this branch runs only when the node
                              -- is partially expanded, i.e. some child is unvisited
                              []    -> error "getUnvisitedChild: no unvisited child"

mctsOneIteration :: STZipper -> State StdGen STZipper
mctsOneIteration root = let (st@(Node ss children), cbs) = pickCandidate root in
  -- playout on candidate (result is from the candidate mover's perspective)
  do
    (result, _) <- playOut $ (getGameState . rootLabel) st
    let v = if result then 1 else 0
        updatedLeaf = Node (SearchState
                            (getGameState ss)
                            ((getReward ss)+v)
                            ((getVisits ss)+1)) children
    -- the leaf got +v for its own mover; the parent gets +(1-v), and so on up
    return $ backProp (1 - v) (updatedLeaf, cbs)

mctsDo :: STZipper -> State StdGen STZipper
mctsDo root@(Node ss _, _)
  | getVisits ss >= 2000 = return root
  | otherwise = mctsOneIteration root >>= mctsDo

mcts :: GameState -> State StdGen GameState
mcts gs =
  do
    (Node _ children, _) <- mctsDo $ makeRootZipper gs
    return $ if null children
               then gs   -- terminal / stuck root: nothing to choose, pass it back
               else getGameState . rootLabel $
                      maximumBy (compare `on` (getVisits . rootLabel)) children
  where
    makeRootZipper (GameState _ players board) = (mkFreshSTNode $ GameState 1 players board, [])

-- ----------------------------------------------------------------------------
-- Anytime / resumable API used by the time-bounded, root-parallel driver in
-- Main. Each worker builds its own tree (its own StdGen), runs 'stepRootN' in
-- batches until a wall-clock deadline, and the driver combines the workers'
-- root-child visit counts.
-- ----------------------------------------------------------------------------

-- | A fresh root tree for @gs@, with the turn normalised to 1 (as 'mcts' does).
mkRootTree :: GameState -> SearchTree
mkRootTree (GameState _ players board) = mkFreshSTNode (GameState 1 players board)

-- | One MCTS iteration from the root, returning the updated root tree.
stepRoot :: SearchTree -> State StdGen SearchTree
stepRoot t = fst <$> mctsOneIteration (t, [])

-- | Run @n@ MCTS iterations from the root.
stepRootN :: Int -> SearchTree -> State StdGen SearchTree
stepRootN n t
  | n <= 0    = return t
  | otherwise = stepRoot t >>= stepRootN (n - 1)

-- | Visit counts of the root's children, in 'getValidNextStates' order. All
-- workers share that order, so the driver can sum these by index.
rootChildVisits :: SearchTree -> [Int]
rootChildVisits (Node _ children) = map (getVisits . rootLabel) children

-- | Game states of the root's children, aligned with 'rootChildVisits'.
rootChildStates :: SearchTree -> [GameState]
rootChildStates (Node _ children) = map (getGameState . rootLabel) children

-- | Force a tree's search work to WHNF. Summing the children's rewards (which
-- depend on rollout outcomes) makes a worker actually run its iterations in its
-- own thread instead of returning a lazy thunk. The Int is meaningless; call it
-- via 'Control.Exception.evaluate'.
rootForce :: SearchTree -> Int
rootForce (Node _ children) =
  sum [ getVisits ss + getReward ss | c <- children, let ss = rootLabel c ]
