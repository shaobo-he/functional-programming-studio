{-# LANGUAGE OverloadedStrings #-}

-- | Line-oriented legal-successor oracle used by the AlphaZero differential
-- tests. Input is one protocol board per line; output is a JSON array of every
-- legal successor according to the authoritative Haskell rules.
module Main (main) where

import Data.Aeson (Value, encode, object, (.=))
import System.IO (BufferMode (LineBuffering), hFlush, hSetBuffering, isEOF, stdout)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

import Santorini.Core
import Santorini.Protocol (decodeBoard, playerToList)

stateValue :: GameState -> Value
stateValue (GameState turn (current, opponent) board) =
  object
    [ "turn" .= turn
    , "players" .= [playerToList current, playerToList opponent]
    , "spaces" .= boardToList board
    ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  loop
  where
    loop = do
      eof <- isEOF
      if eof
        then pure ()
        else do
          line <- BC.getLine
          case decodeBoard (BLC.fromStrict line) of
            Nothing -> BLC.putStrLn "null"
            Just state -> BLC.putStrLn (encode (map stateValue (getValidNextStates state)))
          hFlush stdout
          loop
