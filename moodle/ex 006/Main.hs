module Main where

import Log
import LogAnalysis
import System.IO

main :: IO ()
main = do  
  s <- getContents
  print (parse s)
  print (whatWentWrong (parse s))
