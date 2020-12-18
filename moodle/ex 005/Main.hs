module Main where

import Transmitter

convert :: [[Char]] -> [Int]
convert = map (\ x -> read x :: Int)

main :: IO ()
main = do
  positions_string <- getLine
  message <- getLine
  
  print ((decode . channel (convert (words positions_string)). encode) message)
  --print $ message
  --print $ positions
  --[read n::Int | n <- words positions_string]