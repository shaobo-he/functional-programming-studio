module MCTS (
  mcts,
) where

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

backProp' :: Int -> STZipper -> STZipper
backProp' v stz@(st, []) = stz -- reached root
backProp' v (st, fcb:rcbs) =
  --let childState = rootLabel st
    let oldState = rootLabel . getSearchTree $ fcb
        newState = SearchState
          (getGameState oldState)
          ((getReward oldState) + v)
          ((getVisits oldState) + 1) in
            ((Node newState $ (getLT fcb)++[st]++(getRT fcb)), rcbs)

backProp :: Int -> STZipper -> STZipper
backProp v stz@(st, []) = stz
backProp v stz = backProp v $ backProp' v stz

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
      if isWinningPlayer (getBoard cgs) (snd (getPlayers cgs))
        then stz
        else let moves = getValidNextStates cgs in
          if length moves == 0
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
                              (l,(f:r)) = span (isVisited . rootLabel) children in
                                (f, l, r)

mctsOneIteration :: STZipper -> State StdGen STZipper
mctsOneIteration root = let (st@(Node ss children), cbs) = pickCandidate root in
  -- playout on candidate
  do
    (result, _) <- playOut $ (getGameState . rootLabel) st
    let v = if result then 1 else 0
        updatedLeaf = Node (SearchState
                            (getGameState ss)
                            ((getReward ss)+v)
                            ((getVisits ss)+1)) children
    return $ backProp v (updatedLeaf, cbs)

mctsDo :: STZipper -> State StdGen STZipper
mctsDo root@(st@(Node ss children), cbs)
  | getVisits ss >= 2000 = return root
  | otherwise = mctsOneIteration root >>= mctsDo

mcts :: GameState -> State StdGen GameState
mcts gs =
  do
    ((Node _ children), _) <- mctsDo $ makeRootZipper gs
    return $ getGameState . rootLabel $ maximumBy (compare `on` (getVisits . rootLabel)) children where
    makeRootZipper (GameState _ players board) = (mkFreshSTNode $ GameState 1 players board, [])

mctsShow :: STZipper -> State StdGen String
mctsShow root =
  do
    root@(st@(Node ss children), cbs) <- mctsDo root
    return $ drawTree $ (show . simplifyNode) <$> st where
      simplifyNode ss = (getReward ss, getVisits ss)

main :: IO()
main = do
  let gs = GameState 1 (((2,3),(4,4)),((3,5),(2,5))) $
            boardFromList [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
  --putStrLn $ intercalate "\n" $ map show $ getValidMoves gs
  putStrLn $ show $ getPlayers . fst $ runState (mcts gs) $ mkStdGen 100
  --putStrLn $ fst $ runState (mctsShow rootZipper) $ mkStdGen 200
