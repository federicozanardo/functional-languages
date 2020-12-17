module LogAnalysis where

import Log
import Text.Read (readMaybe)

drops :: [Char] -> Int -> [Char]
drops xs = dropsFun xs 0

dropsFun :: [Char] -> Int -> Int -> [Char]
dropsFun [] _ _ = []
dropsFun (x:xs) i n = if i < n then dropsFun xs (i + 1) n else x : dropsFun xs (i + 1) n

isInfoOrWarning :: [Char] -> Bool
isInfoOrWarning t | t == "I" || t == "W" = True
                  | otherwise = False

buildTypeMessage :: [Char] -> [Char] -> [Char] -> LogMessage
buildTypeMessage err timestamp xs = if (readMaybe timestamp :: Maybe Int) == Nothing
                                      then Unknown (err ++ " " ++ timestamp ++ " " ++ xs)
                                      else mapToTypeMessage err (read timestamp :: Int) xs

mapToTypeMessage :: [Char] -> TimeStamp -> [Char] -> LogMessage
mapToTypeMessage t n xs   | t == "I" = LogMessage Info n xs
                          | t == "W" = LogMessage Warning n xs

isError :: [Char] -> Bool
isError t | t == "E" = True
          | otherwise = False

buildErrorMessage :: [Char] -> [Char] -> [Char] -> [Char] -> LogMessage
buildErrorMessage err errorCode timestamp xs =  if (readMaybe errorCode :: Maybe Int) == Nothing || (readMaybe timestamp :: Maybe Int) == Nothing
                                                  then Unknown (err ++ " " ++ errorCode ++ " " ++ timestamp ++ " " ++ xs)
                                                  else LogMessage (Error (read errorCode :: Int)) (read timestamp :: Int) xs

parseMessage :: [Char] -> LogMessage
parseMessage xs | isInfoOrWarning (head (words xs)) == True = buildTypeMessage (head (words xs)) (words xs !! 1) (drops xs (length (head (words xs)) + length (words xs !! 1) + 2))
                | isError (head (words xs)) == True = buildErrorMessage (head (words xs)) (words xs !! 1) (words xs !! 2) (drops xs (length (head (words xs)) + length (words xs !! 1) + length (words xs !! 2) + 3))
                | otherwise = Unknown xs

parse :: String -> [LogMessage]
parse xs = parseFun (lines xs)

parseFun :: [String] -> [LogMessage]
parseFun = map parseMessage

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert mex Leaf = Node Leaf mex Leaf
insert (LogMessage mexType ts str) (Node left (LogMessage nodeMexType nodeTs nodeStr) right)  | ts <= nodeTs = Node (insert (LogMessage mexType ts str) left) (LogMessage nodeMexType nodeTs nodeStr) right
                                                                                              | otherwise = Node left (LogMessage nodeMexType nodeTs nodeStr) (insert (LogMessage mexType ts str) right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left mex right) = inOrder left ++ [mex] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong mx = [str | (LogMessage (Error level) _ str) <- (inOrder . build) mx, level >= 50]
