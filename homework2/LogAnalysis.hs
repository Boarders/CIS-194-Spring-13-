{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char

parseMessage :: String -> LogMessage

parseMessage st
  | errorType == "E" = LogMessage (Error $ read errorNumber) timeStampInt messageString
  | errorType == "I" = LogMessage Info timeStampInt messageString
  | errorType == "W" = LogMessage Warning timeStampInt messageString
  | otherwise = Unknown st
    where messageList = words st
          errorType = head (messageList)
          errorNumber = if (errorType=="E") then (head.tail.tail) messageList
                          else "-1"         
          timeStampString = (head.tail) messageList
          timeStampInt = if (all isDigit timeStampString) then
                           (read timeStampString)
                         else error "timestamp not given"
          messageString = if (errorType=="E") then (unwords.tail.tail.tail) messageList
                          else (unwords.tail.tail) messageList

parse :: String -> [LogMessage]
parse = map (parseMessage).lines

extractTimeStamp :: LogMessage -> TimeStamp
extractTimeStamp (LogMessage _ t _) = t

extractString :: LogMessage -> String
extractString (LogMessage _ _ st) = st


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown xs) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg @ (LogMessage msgType timeStamp _) tree @ (Node leftTree msg' rightTree) =
  if (t < t') then Node (insert msg leftTree) msg' rightTree
  else Node leftTree msg' (insert msg rightTree)
    where t=extractTimeStamp msg
          t' = extractTimeStamp msg'

build :: [LogMessage] -> MessageTree
build = foldr (\m t -> insert m t) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = (inOrder leftTree) ++ [msg] ++ (inOrder rightTree)

severeError :: LogMessage -> Bool
severeError (LogMessage (Error _) n _) = (n >= 50)
severeError _ = False


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ls = (f.inOrder.build) errorList
  where errorList = filter (severeError) ls
        f = map (extractString)

whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' [] =[]
whatWentWrong' ls = (f.inOrder.build) ls
  where f ls = map (\msg -> (extractString (msg)) ++ "/n") ls

  
    
  
            
