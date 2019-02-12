module MCTS where

import Control.Monad
import Data.Tree
import Data.List (maximumBy)
import Data.Function (on)
import Control.Monad.State.Lazy
import System.Random
import Santorini

data SearchState = SearchState { getGameState :: GameState
                               , getReward :: Int
                               , getVisits :: Int
                               } deriving (Show)

type SearchTree = Tree SearchState

data STCrumb = STCrumb { getSearchTree :: SearchTree
                       , getLT :: [SearchTree]
                       , getRT :: [SearchTree]
                       } deriving (Show)

type STZipper = (SearchTree, [STCrumb])

backProp' :: STZipper -> STZipper
backProp' stz@(st, []) = stz -- reached root
backProp' (st, fcb:rcbs) =
  let childState = rootLabel st
      oldState = rootLabel . getSearchTree $ fcb
      newState = SearchState
        (getGameState oldState)
        ((getReward oldState) + (getReward childState))
        ((getVisits oldState) + (getVisits childState)) in
    ((Node newState $ (getLT fcb)++[st]++(getRT fcb)), rcbs)

backProp :: STZipper -> STZipper
backProp stz@(st, []) = stz
backProp stz = backProp $ backProp' stz

--addChild :: SearchState -> STZipper -> STZipper
--addChild ss (st, cbs) =
--  let newNode = Node ss []
--      parent = Node (rootLabel st) (newNode:(subForest st)) in
--    (newNode, (STCrumb parent [] (subForest st)):cbs)

isVisited :: SearchState -> Bool
isVisited (SearchState _ _ visits) = visits > 0

isFullyExpanded :: SearchTree -> Bool
isFullyExpanded (Node _ []) = False
isFullyExpanded (Node _ children)  = all (isVisited . rootLabel) children

isUnexpanded :: SearchTree -> Bool
isUnexpanded (Node _ children) = (length children) == 0

mkFreshSearchState :: GameState -> SearchState
mkFreshSearchState gs = SearchState gs 0 0

mkFreshSTNode :: GameState -> SearchTree
mkFreshSTNode gs = Node (mkFreshSearchState gs) []

getUCT :: SearchTree -> [Double]
getUCT (Node ss children) = getUCTOfChild <$> children where
  rootVisits = fromIntegral . getVisits $ ss
  getUCTOfChild child = let css = rootLabel child
                            reward = fromIntegral . getReward $ css
                            visits = fromIntegral . getVisits $ css in
                              (reward/visits) +
                              (sqrt $ (2*(log rootVisits))/visits)

pickCandidate :: STZipper -> STZipper
pickCandidate stz@(st, cbs)
  -- unexpanded node -> expands it and then pick
  | isUnexpanded st =
    let cgs = getGameState . rootLabel $ st in
      if isWinningPlayer (getBoard cgs) (fst (getPlayers cgs)) then stz else pickCandidate addChildren
  -- fully expanded node -> select best child
  | isFullyExpanded st = pickCandidate getChildByBestUCT
  -- get the first unvisited child
  | otherwise = let (child, l, r) = getUnvisitedChild in
      (child, (STCrumb st l r):cbs)
    where
      addChildren = let cgs = getGameState . rootLabel $ st in
       ((Node (rootLabel st) (mkFreshSTNode <$> (getValidNextStates cgs))), cbs)
      getChildByBestUCT = let children = subForest st
                              ucts = getUCT st
                              bestId = snd $ maximumBy (compare `on` fst) $ zip ucts [0..] in
                              (children !! bestId,
                                (STCrumb st (take bestId children) (drop (bestId+1) children)):cbs)
      getUnvisitedChild = let children = subForest st
                              (l,(f:r)) = span (isVisited . rootLabel) children in
                                (f, l, r)

mctsOneIteration :: STZipper -> State StdGen STZipper
mctsOneIteration root = let (st@(Node ss children), cbs) = pickCandidate root in
  -- playout on candidate
  do
    (result, _) <- playOut $ (getGameState . rootLabel) st
    let updatedLeaf = Node (SearchState
                            (getGameState ss)
                            ((getReward ss)+(if result then 1 else 0))
                            ((getVisits ss)+1)) children
    return $ backProp (updatedLeaf, cbs)

mctsDo :: STZipper -> State StdGen STZipper
mctsDo root@(st@(Node ss children), cbs)
  | getVisits ss >= 1000 = return root
  | otherwise = mctsOneIteration root >>= mctsDo

mcts :: STZipper -> State StdGen GameState
mcts root =
  do
    root@(st@(Node ss children), cbs) <- mctsDo root
    return $ getGameState . rootLabel $ maximumBy (compare `on` (getVisits . rootLabel)) children

main :: IO()
main = do
  let gs = GameState 1 (((2,3),(4,4)),((3,5),(2,5))) $
            boardFromList [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
      root = mkFreshSTNode gs
      rootZipper = (root, [])
  --putStrLn $ intercalate "\n" $ map show $ getValidMoves gs
  putStrLn $ show $ fst $ runState (mcts rootZipper) $ mkStdGen 100
