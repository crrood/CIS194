{-# OPTIONS_GHC -Wall #-}
module Main where

import Log
import Text.Read
import Data.Maybe

-- Parsing methods

parseMessageType :: [String] -> MessageType
parseMessageType (code:severity:_)
  | code == "I" = Info
  | code == "W" = Warning
  -- should add type checking for the Int here
  | code == "E" = Error (read severity :: Int)
  | otherwise   = error "invalid message type code"
parseMessageType _ = error "incorrect number of message type values"

parseTimestamp :: [String] -> Int -> Int
parseTimestamp messageArray messageTypeLength =
  read (messageArray !! messageTypeLength) :: Int

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

-- Utility functions

isUnknown :: [String] -> Bool
isUnknown message
  | code == "I" || code == "W"              = False
  | code == "E" && not (isNothing severity) = False
  | otherwise                               = True
  where code      = message !! 0
        severity  = readMaybe (message !! 1) :: Maybe Int

-- Binary search tree

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node leftTree nodeMessage rightTree)
  | newTimestamp < nodeTimestamp  = Node (insert logMessage leftTree) nodeMessage rightTree
  | otherwise                     = Node leftTree nodeMessage (insert logMessage rightTree)
  where newTree         = insert logMessage Leaf
        newTimestamp    = getTimestamp newTree
        nodeTimestamp   = getTimestamp (insert nodeMessage Leaf)

getTimestamp :: MessageTree -> Int
getTimestamp Leaf = 0
getTimestamp (Node _ (LogMessage _ timestamp _) _) = timestamp
getTimestamp _ = error "bad timestamp in tree"

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (logMessage:[]) = insert logMessage Leaf
build (logMessage:logs) = insert logMessage (build logs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf logMessage Leaf) = [logMessage]
inOrder (Node leftTree logMessage rightTree) =
  (inOrder leftTree ++ [logMessage] ++ inOrder rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error severity) _ message):logs)
  | severity > 50 = ([message] ++ (whatWentWrong logs))
  | otherwise     = whatWentWrong logs
whatWentWrong (_:logs) = whatWentWrong logs

-- Input / output

main :: IO ()
main = do
  putStrLn (show (
    (parseMessage "W 9836 mice in the air") ==
    (LogMessage Warning 9836 "mice in the air")))

  putStrLn (show (
    parseMessage "Unknown format" ==
    Unknown "Unknown format"))

  let logMessage = parseMessage "E 2 562 help help"
  putStrLn (show (
    logMessage ==
    (LogMessage (Error 2) 562 "help help")))

  let rootNode = insert logMessage Leaf
  putStrLn (show (
    rootNode ==
    Node Leaf (LogMessage (Error 2) 562 "help help") Leaf))

  let rightTree = insert (parseMessage "W 9836 mice in the air") (insert logMessage rootNode)
  putStrLn (show (
    rightTree ==
    Node Leaf (LogMessage (Error 2) 562 "help help") (Node Leaf (LogMessage (Error 2) 562 "help help") (Node Leaf (LogMessage Warning 9836 "mice in the air") Leaf))))

  let leftTree = insert (parseMessage "W 1 mice in the air") (insert (parseMessage "W 2 mice in the air") rootNode)
  putStrLn (show (
    leftTree ==
    Node (Node (Node Leaf (LogMessage Warning 1 "mice in the air") Leaf) (LogMessage Warning 2 "mice in the air") Leaf) (LogMessage (Error 2) 562 "help help") Leaf))

  let fullTree = (build [logMessage, (parseMessage "E 2 100 help help"), (parseMessage "W 1000 help help"), (parseMessage "I 150 help help")])
  putStrLn (show (
    fullTree ==
    Node (Node Leaf (LogMessage (Error 2) 100 "help help") Leaf) (LogMessage Info 150 "help help") (Node (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf) (LogMessage Warning 1000 "help help") Leaf)))

  let inOrderList = inOrder fullTree
  putStrLn (show (
    inOrderList ==
    [LogMessage (Error 2) 100 "help help",LogMessage Info 150 "help help",LogMessage (Error 2) 562 "help help",LogMessage Warning 1000 "help help"]))
