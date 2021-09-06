{-# OPTIONS_GHC -Wall #-}
module Main where

import Log
import Text.Read
import Data.Maybe

parseMessageType :: [String] -> MessageType
parseMessageType (code:severity:_)
  | code == "I" = Info
  | code == "W" = Warning
  -- should add type checking for the Int here
  | code == "E" = Error (read severity :: Int)
  | otherwise   = error "invalid message type code"
parseMessageType _ = error "incorrect number of message type values"

isError :: MessageType -> Bool
isError messageType =
  case messageType of
    Error _ -> True
    _       -> False

parseTimestamp :: [String] -> Int -> Int
parseTimestamp messageArray messageTypeLength =
  read (messageArray !! messageTypeLength) :: Int

isUnknown :: [String] -> Bool
isUnknown message
  | code == "I" || code == "W"        = False
  | code == "E" && isNothing severity = True
  | otherwise                         = True
  where code      = message !! 0
        severity  = readMaybe (message !! 1) :: Maybe Int

parseMessage :: String -> LogMessage
parseMessage message
  | isUnknown messageArray  = Unknown message
  | otherwise =
    LogMessage
    messageType
    timestamp
    (unwords (snd (splitAt (messageTypeLength + 1) (messageArray))))

  where messageArray  = words message
        messageType   = parseMessageType messageArray
        messageTypeLength =
          case messageType of
          Error _ -> 2
          _       -> 1
        timestamp     = parseTimestamp messageArray messageTypeLength

parse :: String -> [LogMessage]
parse logFile =
  map parseMessage (lines logFile)

main :: IO ()
main = do
  putStrLn (show (parseMessage "E 2 562 help help"))
  putStrLn (show (parseMessage "W 9836 mice in the air"))
  putStrLn (show (parseMessage "Unknown format"))
  -- putStrLn (show (testParse parse 10 "error.log"))
