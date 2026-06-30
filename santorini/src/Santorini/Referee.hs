{-# LANGUAGE ScopedTypeVariables #-}

-- | The referee / driver. It spawns two protocol-speaking player programs,
-- runs the placement handshake, relays board states, VALIDATES every move
-- against the rules, detects win / stalemate, and forcibly terminates both
-- processes when the game ends.
--
-- It also keeps a full transcript of every step each player makes (placements
-- plus each move\/build), which it prints and writes to a log file.
module Santorini.Referee
  ( RefConfig(..)
  , defaultConfig
  , runReferee
  ) where

import Control.Exception (IOException, catch, try)
import Control.Monad (forM)
import Data.List (find, nub)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.FilePath (takeFileName)
import System.IO
  (BufferMode (LineBuffering), Handle, hClose, hFlush, hGetLine, hSetBuffering, stdout)
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Process
  ( CreateProcess (create_group, env, std_in, std_out), ProcessHandle, StdStream (CreatePipe)
  , createProcess, getPid, proc, waitForProcess )
import System.Timeout (timeout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BLC

import Santorini.Core
import Santorini.Protocol (decodeBoard, decodePlacement, encodeBoard, placementToPlayers)

-- ----------------------------------------------------------------------------
-- Configuration
-- ----------------------------------------------------------------------------

data RefConfig = RefConfig
  { cmdA       :: FilePath        -- ^ executable for contestant A
  , cmdB       :: FilePath        -- ^ executable for contestant B
  , nGames     :: Int             -- ^ games to play (first move alternates)
  , thinkMs    :: Int             -- ^ per-move think budget handed to each player (ms)
  , plyCap     :: Int             -- ^ max plies before declaring a draw
  , logPath    :: FilePath        -- ^ human-readable transcript file
  , boardsPath :: Maybe FilePath  -- ^ if set, full board state per ply as JSONL
  }

defaultConfig :: FilePath -> FilePath -> RefConfig
defaultConfig a b = RefConfig
  { cmdA = a, cmdB = b, nGames = 2
  , thinkMs = 1000, plyCap = 400, logPath = "referee-log.txt", boardsPath = Nothing }

-- | Hard per-move wall-clock limit (microseconds): the think budget plus a
-- generous grace margin for process startup / GC. Exceeding it is a loss.
hardMicros :: RefConfig -> Int
hardMicros cfg = (thinkMs cfg * 2 + 1000) * 1000

-- ----------------------------------------------------------------------------
-- Process plumbing
-- ----------------------------------------------------------------------------

data PlayerProc = PlayerProc
  { ppName :: String
  , ppIn   :: Handle
  , ppOut  :: Handle
  , ppPh   :: ProcessHandle
  }

spawnPlayer :: RefConfig -> FilePath -> IO PlayerProc
spawnPlayer cfg cmd = do
  parentEnv <- getEnvironment
  let childEnv = ("SANTORINI_TIME_MS", show (thinkMs cfg)) : parentEnv
  (Just hin, Just hout, _, ph) <-
    createProcess (proc cmd [])
      { std_in = CreatePipe, std_out = CreatePipe, env = Just childEnv
      , create_group = True }   -- own process group, so we can kill descendants too
  hSetBuffering hin  LineBuffering
  hSetBuffering hout LineBuffering
  pure (PlayerProc (takeFileName cmd) hin hout ph)

-- | Tear a player down without ever blocking: close its stdin (EOF for a
-- well-behaved bot), SIGTERM it, wait at most ~1s, then escalate to SIGKILL for
-- a bot that ignores SIGTERM. Finally close the pipe handles.
terminatePlayer :: PlayerProc -> IO ()
terminatePlayer p = do
  hClose (ppIn p) `catch` ignore                              -- EOF for a well-behaved bot
  mpid <- getPid (ppPh p)                                     -- group leader pid (create_group)
  case mpid of
    Nothing  -> pure ()                                       -- already exited
    Just pid -> do
      signalProcessGroup sigTERM pid `catch` ignore           -- polite, whole group
      reaped <- timeout 1000000 (waitForProcess (ppPh p))
      case reaped of
        Just _  -> pure ()
        Nothing -> do
          signalProcessGroup sigKILL pid `catch` ignore       -- force-kill the group (incl. children)
          _ <- (try (waitForProcess (ppPh p)) :: IO (Either IOException ExitCode))
          pure ()
  hClose (ppOut p) `catch` ignore
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()

sendLine :: Handle -> BLC.ByteString -> IO Bool
sendLine h bs =
  (BLC.hPutStrLn h bs >> pure True) `catch` \(_ :: IOException) -> pure False

readLineT :: Int -> Handle -> IO (Maybe String)
readLineT micros h = do
  r <- timeout micros (try (hGetLine h) :: IO (Either IOException String))
  pure $ case r of
    Just (Right l) -> Just l
    _              -> Nothing

-- ----------------------------------------------------------------------------
-- Transcript types
-- ----------------------------------------------------------------------------

data Placement = Placement { plName :: String, plSide :: Char, plW1 :: Pos, plW2 :: Pos }

data Step = Step
  { stName  :: String
  , stSide  :: Char
  , stTurn  :: Int
  , stFrom  :: Pos
  , stTo    :: Pos
  , stBuild :: Maybe Pos
  , stWin   :: Bool
  , stMs    :: Int        -- ^ wall-clock the player took for this move
  }

data GameLog = GameLog
  { glIndex      :: Int
  , glFirst      :: Char
  , glPlacements :: [Placement]
  , glSteps      :: [Step]
  , glWinner     :: Maybe Char
  , glReason     :: String
  , glBoards     :: [GameState]   -- ^ initial post-placement state, then after each ply
  }

-- ----------------------------------------------------------------------------
-- Rules glue
-- ----------------------------------------------------------------------------

allCells :: [Pos]
allCells = [ (r, c) | r <- [1 .. 5], c <- [1 .. 5] ]

inBounds :: Pos -> Bool
inBounds (r, c) = r >= 1 && r <= 5 && c >= 1 && c <= 5

-- | The canonical legal successor matching what the player returned (compared
-- on players + spaces), or 'Nothing' if the player's board is not a legal move.
legalNext :: GameState -> GameState -> Maybe GameState
legalNext s s' =
  find (\t -> getPlayers t == getPlayers s'
              && boardToList (getBoard t) == boardToList (getBoard s'))
       (getValidNextStates s)

-- | (from, to) of the worker that moved, given the active player's workers
-- before the move and after it.
movedWorker :: Player -> Player -> (Pos, Pos)
movedWorker (a1, a2) (b1, b2)
  | a1 /= b1  = (a1, b1)
  | otherwise = (a2, b2)

-- | The cell whose level rose by one (the build), if any.
buildDiff :: Board -> Board -> Maybe Pos
buildDiff before after =
  find (\p -> getLevel p after == getLevel p before + 1) allCells

-- ----------------------------------------------------------------------------
-- Placement handshake
-- ----------------------------------------------------------------------------

-- | Run the two-message placement handshake. Returns @Left (winnerSide, reason)@
-- if a player produced an illegal placement, else the initial board + records.
placementPhase
  :: RefConfig
  -> (PlayerProc, Char)
  -> (PlayerProc, Char)
  -> IO (Either (Char, String) (GameState, [Placement]))
placementPhase cfg (fp, fside) (sp, sside) = do
  _  <- sendLine (ppIn fp) (BLC.pack "[]")
  mF <- readLineT (hardMicros cfg) (ppOut fp)
  case mF of
    Nothing -> pure (Left (sside, ppName fp ++ " timed out during placement"))
    Just fLine ->
      case decodePlacement (BLC.pack fLine) >>= placementToPlayers of
        Just [fw] | okOne fw -> do
          _  <- sendLine (ppIn sp) (BLC.pack fLine)   -- relay first's placement verbatim
          mS <- readLineT (hardMicros cfg) (ppOut sp)
          case mS of
            Nothing -> pure (Left (fside, ppName sp ++ " timed out during placement"))
            Just sLine ->
              case decodePlacement (BLC.pack sLine) >>= placementToPlayers of
                Just [_echo, sw] | okBoth fw sw -> do
                  let s0  = GameState 0 (fw, sw) emptyBoard
                      pls = [ Placement (ppName fp) fside (fst fw) (snd fw)
                            , Placement (ppName sp) sside (fst sw) (snd sw) ]
                  pure (Right (s0, pls))
                _ -> pure (Left (fside, ppName sp ++ " returned an invalid placement"))
        _ -> pure (Left (sside, ppName fp ++ " returned an invalid placement"))
  where
    okOne (w1, w2) = all inBounds [w1, w2] && w1 /= w2
    okBoth (a1, a2) (b1, b2) =
      let four = [a1, a2, b1, b2] in all inBounds four && length (nub four) == 4

-- ----------------------------------------------------------------------------
-- Play loop
-- ----------------------------------------------------------------------------

-- | Drive one game from the post-placement state. @active@ is the player to
-- move (its workers are @fst (getPlayers s)@).
playLoop
  :: RefConfig
  -> GameState
  -> (PlayerProc, Char)            -- active
  -> (PlayerProc, Char)            -- other
  -> Int                           -- ply
  -> [Step]                        -- accumulated steps (reversed)
  -> [GameState]                   -- accumulated post-move boards (reversed)
  -> IO (Maybe Char, String, [Step], [GameState])
playLoop cfg s active@(ap, aside) other@(_, oside) ply acc bacc
  | ply > plyCap cfg =
      pure (Nothing, "ply cap reached -> draw", reverse acc, reverse bacc)
  | null (getValidNextStates s) =
      pure (Just oside, ppName ap ++ " (" ++ [aside] ++ ") has no legal move", reverse acc, reverse bacc)
  | otherwise = do
      t0   <- getCurrentTime
      sent <- sendLine (ppIn ap) (encodeBoard s)
      if not sent
        then pure (Just oside, ppName ap ++ " (" ++ [aside] ++ ") closed its input", reverse acc, reverse bacc)
        else do
          mresp <- readLineT (hardMicros cfg) (ppOut ap)
          t1    <- getCurrentTime
          let elapsedMs = round (realToFrac (diffUTCTime t1 t0) * 1000 :: Double) :: Int
          case mresp of
            Nothing ->
              pure (Just oside, ppName ap ++ " (" ++ [aside] ++ ") timed out (> "
                                ++ show (hardMicros cfg `div` 1000) ++ "ms) or crashed", reverse acc, reverse bacc)
            Just line ->
              case decodeBoard (BLC.pack line) of
                Nothing ->
                  pure (Just oside, ppName ap ++ " returned an unparseable board", reverse acc, reverse bacc)
                Just s' ->
                  case legalNext s s' of
                    Nothing ->
                      pure (Just oside, ppName ap ++ " (" ++ [aside] ++ ") made an ILLEGAL move", reverse acc, reverse bacc)
                    Just t -> do
                      let (frm, to) = movedWorker (fst (getPlayers s)) (snd (getPlayers t))
                          bld       = buildDiff (getBoard s) (getBoard t)
                          won       = moverWon t
                          step      = Step (ppName ap) aside (getTurn t) frm to bld won elapsedMs
                          acc'      = step : acc
                          bacc'     = t : bacc
                      if won
                        then pure (Just aside, ppName ap ++ " (" ++ [aside] ++ ") reached level 3", reverse acc', reverse bacc')
                        else playLoop cfg t other active (ply + 1) acc' bacc'

-- ----------------------------------------------------------------------------
-- One game
-- ----------------------------------------------------------------------------

playOneGame :: RefConfig -> Int -> IO GameLog
playOneGame cfg gi = do
  a <- spawnPlayer cfg (cmdA cfg)
  b <- spawnPlayer cfg (cmdB cfg)
  let aSide = (a, 'A')
      bSide = (b, 'B')
      firstIsA = odd gi
      (first, second) = if firstIsA then (aSide, bSide) else (bSide, aSide)
      fside = snd first
  ep <- placementPhase cfg first second
  result <- case ep of
    Left (winner, reason) ->
      pure (GameLog gi fside [] [] (Just winner) reason [])
    Right (s0, pls) -> do
      (mw, reason, steps, boards) <- playLoop cfg s0 first second 1 [] []
      pure (GameLog gi fside pls steps mw reason (s0 : boards))
  terminatePlayer a
  terminatePlayer b
  pure result

-- ----------------------------------------------------------------------------
-- Top level
-- ----------------------------------------------------------------------------

runReferee :: RefConfig -> IO ()
runReferee cfg = do
  let nameA' = takeFileName (cmdA cfg)
      nameB' = takeFileName (cmdB cfg)
      header = renderHeader cfg nameA' nameB'
  putStr header
  hFlush stdout
  case boardsPath cfg of            -- truncate the boards file up front
    Just bp -> writeFile bp ""
    Nothing -> pure ()
  -- Print each game's transcript as it finishes, so a later game's trouble
  -- can never erase results already played.
  logs <- forM [1 .. nGames cfg] $ \gi -> do
    g <- playOneGame cfg gi
    putStr (renderGame g)
    hFlush stdout
    case boardsPath cfg of
      Just bp -> appendFile bp (renderBoards gi (glBoards g))
      Nothing -> pure ()
    pure g
  let summary = renderSummary nameA' nameB' logs
  putStr summary
  writeFile (logPath cfg) (header ++ concatMap renderGame logs ++ summary)
  printf "\n(transcript written to %s)\n" (logPath cfg)
  case boardsPath cfg of
    Just bp -> printf "(per-ply board states written to %s)\n" bp
    Nothing -> pure ()

-- ----------------------------------------------------------------------------
-- Rendering
-- ----------------------------------------------------------------------------

showPos :: Pos -> String
showPos (r, c) = printf "(%d,%d)" r c

renderStep :: Step -> String
renderStep st =
  printf "    turn %3d  %-16s (%c)  move %s -> %s   %-26s%6dms%s\n"
    (stTurn st) (stName st) (stSide st)
    (showPos (stFrom st)) (showPos (stTo st))
    (maybe "(winning step, no build)" (\p -> "build " ++ showPos p) (stBuild st))
    (stMs st)
    (if stWin st then "   <== WIN" else "")

renderPlacement :: Placement -> String
renderPlacement p =
  printf "    place     %-16s (%c)  %s %s\n"
    (plName p) (plSide p) (showPos (plW1 p)) (showPos (plW2 p))

renderGame :: GameLog -> String
renderGame g =
  printf "Game %d  (first move: side %c)\n" (glIndex g) (glFirst g)
    ++ concatMap renderPlacement (glPlacements g)
    ++ concatMap renderStep (glSteps g)
    ++ printf "  -> %s | %d plies | %s\n\n"
         (maybe "draw" (\w -> "side " ++ [w] ++ " wins") (glWinner g))
         (length (glSteps g))
         (glReason g)

-- | Full board state after each ply, as JSONL: one
-- @{"game":G,"ply":P,"board":{turn,players,spaces}}@ object per line. Ply 0 is
-- the initial post-placement position. Lets any later analysis read positions
-- directly without replaying moves.
renderBoards :: Int -> [GameState] -> String
renderBoards gi states = unlines
  [ "{\"game\":" ++ show gi ++ ",\"ply\":" ++ show ply
    ++ ",\"board\":" ++ BLC.unpack (encodeBoard st) ++ "}"
  | (ply, st) <- zip [0 :: Int ..] states ]

renderHeader :: RefConfig -> String -> String -> String
renderHeader cfg nameA' nameB' =
  printf "Santorini referee : A=%s  vs  B=%s   (%d games, %dms/move, hard limit %dms)\n\n"
         nameA' nameB' (nGames cfg) (thinkMs cfg) (hardMicros cfg `div` 1000)

renderSummary :: String -> String -> [GameLog] -> String
renderSummary nameA' nameB' logs =
  printf "Summary: A (%s) %d   B (%s) %d   draws %d\n" nameA' aWins nameB' bWins draws
  where
    aWins = length (filter ((== Just 'A') . glWinner) logs)
    bWins = length (filter ((== Just 'B') . glWinner) logs)
    draws = length logs - aWins - bWins
