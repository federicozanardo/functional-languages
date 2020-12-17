module Main where

import Validate
import Data.Char (digitToInt)

main :: IO ()

fromStringToIntList :: String -> [Int]
fromStringToIntList [] = []
fromStringToIntList (x:xs) | x == ' ' = fromStringToIntList xs
                           | otherwise = digitToInt x : fromStringToIntList xs


fromListStringToIntList :: [String] -> [[Int]]
fromListStringToIntList xs = map fromStringToIntList xs

convertToIntegerFun :: [Int] -> Int -> Int
convertToIntegerFun [] _ = 0
convertToIntegerFun (x:xs) i = 10^i * x + convertToIntegerFun xs (i - 1)

convertToInteger :: [Int] -> Int
convertToInteger xs = convertToIntegerFun xs (length xs - 1)

convertFromIntListToInteger :: [[Int]] -> [Int]
convertFromIntListToInteger xs = map convertToInteger xs

validateCreditCardNumber :: [Int] -> [Bool]
validateCreditCardNumber [] = []
validateCreditCardNumber (x:xs) = validateCreditCardNumber xs ++ [validate x]

loop :: [String] -> IO()
loop credit_card_number = do
    value <- getLine

    if value == "end"
    then print $ validateCreditCardNumber (convertFromIntListToInteger (fromListStringToIntList credit_card_number))
    else
      loop (value : credit_card_number)

main = loop []