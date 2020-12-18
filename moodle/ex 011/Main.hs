module Main where

import System.IO
import Parse

expr' :: Parser Int
expr' = do  x <- natural
            xs <- many (do symbol "-"
                           natural)
            return (foldl (-) x xs)

main :: IO ()
main = do input <- getLine
          print (parse expr' input)
