-- | Lightweight test suite for the Santorini engine (no external test deps;
-- exits non-zero on any failure so `cabal test` / CI catch regressions).
module Main (main) where

import Data.List (nub)
import System.Exit (exitFailure)

import Santorini.Core
import Santorini.Engine.Modern
  ( Engine
  , MNode
  , RolloutJob
  , base
  , cancelRollout
  , chooseMove
  , chooseRootChild
  , enh
  , finishRollout
  , mkRoot
  , nodePending
  , nodeState
  , nodeVisits
  , reserveRollout
  , reuseRoot
  , runRolloutJob
  )
import Santorini.Protocol (decodeBoard, encodeBoard)
import Santorini.Search (timedShared)

-- A position where the player to move can step a worker from level 2 onto the
-- adjacent level-3 cell (1,2) and win immediately.
matePos :: GameState
matePos =
  GameState 1 (((1, 1), (5, 5)), ((3, 3), (3, 4)))
    (setLevels [((1, 1), 2), ((1, 2), 3)] emptyBoard)

openingPos :: GameState
openingPos = GameState 1 (((2, 2), (4, 4)), ((2, 4), (4, 2))) emptyBoard

-- The opponent threatens (1,1) level 2 -> (1,2) level 3. The player to move
-- cannot climb onto (1,2), but can move near it and build the required dome.
mustDefendPos :: GameState
mustDefendPos =
  GameState 1 (((2, 2), (5, 5)), ((1, 1), (4, 4)))
    (setLevels [((1, 1), 2), ((1, 2), 3)] emptyBoard)

defendsImmediateWin :: GameState -> Bool
defendsImmediateWin = not . hasWinningMove

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ name)
  pure ok

reserveMany
  :: Int
  -> Engine
  -> MNode
  -> Gen
  -> ([RolloutJob], MNode, Gen)
reserveMany count eng = go count []
  where
    go 0 jobs tree gen = (reverse jobs, tree, gen)
    go n jobs tree gen =
      let ((job, tree'), gen') = runSt (reserveRollout eng tree) gen
      in case job of
           Just reserved -> go (n - 1) (reserved : jobs) tree' gen'
           Nothing -> (reverse jobs, tree', gen')

finishMany
  :: Engine
  -> [RolloutJob]
  -> MNode
  -> Gen
  -> (MNode, Gen)
finishMany _ [] tree gen = (tree, gen)
finishMany eng (job : jobs) tree gen =
  let (value, gen') = runSt (runRolloutJob eng job) gen
  in finishMany eng jobs (finishRollout eng job value tree) gen'

main :: IO ()
main = do
  let -- the modern engine (with the solver) must find the mate-in-1
      mateResult = fst (runSt (chooseMove enh 200 matePos) (mkGen 1))
      -- Root selection must never hand over a mate-in-1 when a defense exists,
      -- even before statistical search has visited any child.
      guardedResult = fst (runSt (chooseMove base 1 mustDefendPos) (mkGen 1))
      diversified =
        nub [ encodeBoard (fst (runSt (chooseMove base 2 openingPos) (mkGen seed)))
            | seed <- [1 .. 8]
            ]
      -- the allocation-free sampler must enumerate exactly getValidMoves
      samplerOK = getValidMoves openingPos
                    == [ nthMove openingPos k | k <- [0 .. moveCount openingPos - 1] ]
      -- the protocol JSON must round-trip a board
      roundTripOK = case decodeBoard (encodeBoard openingPos) of
        Just gs -> getPlayers gs == getPlayers openingPos
                   && boardToList (getBoard gs) == boardToList (getBoard openingPos)
                   && getTurn gs == getTurn openingPos
        Nothing -> False
      -- every legal successor advances the turn by one
      turnsOK = all (\t -> getTurn t == getTurn openingPos + 1)
                    (getValidNextStates openingPos)
      -- the opening has legal moves and the counts agree
      countOK = moveCount openingPos == length (getValidMoves openingPos)
                && moveCount openingPos > 0
      (jobs, reservedRoot, rolloutGen) =
        reserveMany 8 enh (mkRoot enh openingPos) (mkGen 31)
      pendingOK = length jobs == 8 && nodePending reservedRoot == 8
      (completedRoot, _) = finishMany enh jobs reservedRoot rolloutGen
      completedOK = nodePending completedRoot == 0
                    && nodeVisits completedRoot == length jobs
      cancellationOK = case jobs of
        first : _ -> nodePending (cancelRollout first reservedRoot) == 7
        [] -> False
      rerootOK = case chooseRootChild enh completedRoot of
        Just child ->
          let reused = reuseRoot enh (nodeState child) (Just completedRoot)
          in nodeState reused == nodeState child
             && nodeVisits reused == nodeVisits child
        Nothing -> False

  (sharedMove, sharedCache) <- timedShared enh 20 2 Nothing openingPos
  let sharedSearchOK = sharedMove `elem` getValidNextStates openingPos
                       && case sharedCache of
                            Just cached -> nodeState cached == sharedMove
                            Nothing -> False

  results <- sequence
    [ check "modern (enh) finds mate-in-1"       (moverWon mateResult)
    , check "root choice blocks immediate loss"   (defendsImmediateWin guardedResult)
    , check "selection seeds diversify root coverage" (length diversified > 1)
    , check "sampler nthMove == getValidMoves"   samplerOK
    , check "protocol board JSON round-trips"     roundTripOK
    , check "successors increment the turn"       turnsOK
    , check "moveCount == length getValidMoves"   countOK
    , check "reservations track outstanding work" pendingOK
    , check "completed rollouts clear pending"    completedOK
    , check "cancel removes one reservation"      cancellationOK
    , check "reroot preserves selected subtree"   rerootOK
    , check "threaded shared search returns legal move" sharedSearchOK
    ]
  if and results then putStrLn "ALL PASS" else exitFailure
