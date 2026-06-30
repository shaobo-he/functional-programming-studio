{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, evaluate, try)
import Control.Monad.State.Lazy
import Data.Aeson
import Data.Function (on)
import Data.List (maximumBy)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Environment (lookupEnv)
import System.IO (stdin, stdout, hFlush)
import System.IO.Error (isEOFError)
import System.Random
import Text.Read (readMaybe)
import qualified Data.ByteString as B (hGetLine, ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (putStrLn, ByteString)
import Santorini.Legacy.Game
import Santorini.Legacy.MCTS

updateTurn :: Int -> GameState -> GameState
updateTurn newTurn (GameState _ players board) =
  GameState newTurn players board

tupleToList :: (Int, Int) -> [Int]
tupleToList (x,y) = [x,y]

listToTuple :: [Int] -> (Int, Int)
listToTuple [x,y] = (x,y)
listToTuple _     = noPos

-- get our two starting positions; rejection-sample two distinct cells
placePlayer1' :: State StdGen [[[Int]]]
placePlayer1' = do
  pos1 <- mkRandPos
  pos2 <- mkRandPos
  if isDiffPos pos1 pos2
    then return $ [[tupleToList pos1, tupleToList pos2]]
    else placePlayer1'

-- place our two workers given the opponent's; avoid the opponent's workers AND
-- our own already-placed worker (the second conjunct on pos2' is the
-- self-overlap fix).
placePlayer2' :: [[[Int]]] -> State StdGen [[[Int]]]
placePlayer2' lst@[[pos1, pos2], [pos1', pos2']]
  | isDiffPos noPos (listToTuple pos1') && isDiffPos noPos (listToTuple pos2') = return lst
  -- pos1' is not done, get it working
  | not $ isDiffPos noPos (listToTuple pos1') = do
    pos <- mkRandPos
    if (isDiffPos pos (listToTuple pos1))
       && (isDiffPos pos (listToTuple pos2))
       && (isDiffPos pos (listToTuple pos2'))   -- don't collide with our other worker
      then placePlayer2' $ [[pos1, pos2], [tupleToList pos, pos2']]
      else placePlayer2' lst
  -- pos2' is not done, switch pos1' and pos2'
  | not $ isDiffPos noPos (listToTuple pos2') = placePlayer2' $ [[pos1, pos2], [pos2', pos1']]
placePlayer2' lst = return lst   -- unexpected shape: leave unchanged

placePlayer :: B.ByteString -> State StdGen (Maybe BL.ByteString)
placePlayer jsonStr = case decodeStrict jsonStr :: Maybe [[[Int]]] of
  Just players -> case players of
                    []       -> Just . encode <$> placePlayer1'
                    [player] -> Just . encode <$> placePlayer2' [player, [[-1,-1],[-1,-1]]]
                    _        -> return Nothing
  Nothing -> return Nothing

-- ----------------------------------------------------------------------------
-- Time-bounded, root-parallel anytime MCTS for a board move.
-- One worker per RTS capability; each grows its own tree until a shared
-- wall-clock deadline, then we combine by summed root-child visits.
-- ----------------------------------------------------------------------------

-- distinct generators for the workers
genSeeds :: StdGen -> [StdGen]
genSeeds g = let (a, b) = split g in a : genSeeds b

chooseTimedParallel :: Int -> GameState -> StdGen -> IO GameState
chooseTimedParallel timeMs gs gen = do
  nCap <- getNumCapabilities
  let nWorkers = max 1 nCap
      seeds    = take nWorkers (genSeeds gen)
  start <- getCurrentTime
  let deadline = addUTCTime (fromIntegral timeMs / 1000) start
  mvars <- mapM (const newEmptyMVar) [1 .. nWorkers]
  mapM_ (\(mv, sd) -> forkIO $ do
           r <- try (worker deadline sd)
           putMVar mv $ case r of
             Left (_ :: SomeException) -> mkRootTree gs   -- never leave the MVar empty
             Right t                   -> t
        ) (zip mvars seeds)
  trees <- mapM takeMVar mvars
  let stateLists = map rootChildStates trees
      states     = case filter (not . null) stateLists of
                     (s : _) -> s
                     []      -> []
      n          = length states
      visitLists = map rootChildVisits trees
      summed     = [ sum [ vl !! i | vl <- visitLists, length vl == n ] | i <- [0 .. n - 1] ]
  pure $ if null states
           then gs
           else states !! snd (maximumBy (compare `on` fst) (zip summed [0 ..]))
  where
    batchSize = 64 :: Int
    -- a worker grows one tree until the deadline (always at least one batch)
    worker deadline = loop (mkRootTree gs)
      where
        loop t g = do
          let (t', g') = runState (stepRootN batchSize t) g
          _   <- evaluate (rootForce t')   -- force this batch's search in the worker
          now <- getCurrentTime
          if now >= deadline then return t' else loop t' g'

-- read one line, returning Nothing on end of input rather than crashing
readLine :: IO (Maybe B.ByteString)
readLine = fmap Just (B.hGetLine stdin) `catch` eofToNothing
  where
    eofToNothing :: IOError -> IO (Maybe B.ByteString)
    eofToNothing e = if isEOFError e then return Nothing else ioError e

play :: Int -> StdGen -> IO ()
play timeMs gen0 = do
  ml <- readLine
  case ml of
    Nothing   -> return ()        -- end of game / input: exit cleanly
    Just line -> do
      let (mplace, gen1) = runState (placePlayer line) gen0
      case mplace of
        Just str -> BL.putStrLn str >> hFlush stdout >> play timeMs gen1
        Nothing  -> case decodeStrict line :: Maybe GameState of
          Just gs -> do
            let (wgen, gen2) = split gen1
            best <- chooseTimedParallel timeMs gs wgen
            BL.putStrLn (encode (updateTurn (getTurn gs + 1) best))
            hFlush stdout
            play timeMs gen2
          Nothing -> play timeMs gen1   -- unparseable line: skip and continue

main :: IO ()
main = do
  mt <- lookupEnv "SANTORINI_TIME_MS"
  let timeMs = maybe 1000 id (mt >>= readMaybe)
  play timeMs (mkStdGen 200)
