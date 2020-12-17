module Main where

import Merge

main :: IO ()

digitToNumber c | c == '0' = 0
                | c == '1' = 1
                | c == '2' = 2
                | c == '3' = 3
                | c == '4' = 4
                | c == '5' = 5
                | c == '6' = 6
                | c == '7' = 7
                | c == '8' = 8
                | c == '9' = 9

conv_fun :: [Char] -> Int -> Int
conv_fun [] _ = 0
conv_fun (x:xs) i = 10^i * digitToNumber x + conv_fun xs (i - 1) 

conv :: [Char] -> Int
conv xs = conv_fun xs (length xs - 1)

convertToInt :: [[Char]] -> [Int]
convertToInt [] = []
convertToInt (x:xs) = conv x : convertToInt xs

fun :: String -> IO()
fun string = do
  value <- getLine
  print $ msort (convertToInt (words value))

main = fun []