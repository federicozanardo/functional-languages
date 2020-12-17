module Main where

import System.IO
import Balance

main :: IO ()
main = do input <- getLine
          print (app balance ([], input))
