module Transmitter where

import Data.Char

type Bit = Int

-- Conversion functions
bin2int :: [Bit] -> Int
bin2int = foldr (\ x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Build byte
makeByte :: [Bit] -> [Bit]
makeByte bits = take 8 (bits ++ repeat 0)

-- Get parity bit + byte [0/1|byte]
chop9bits :: [Bit] -> [[Bit]]
chop9bits [] = []
chop9bits bits = take 9 bits : chop9bits (drop 9 bits)

-- Parity bit
addParityBit :: [Bit] -> [Bit]
addParityBit bits = sum bits `mod` 2 : bits

parityCheck :: [Bit] -> [Bit]
parityCheck (x:xs) | sum xs `mod` 2 == x = xs
                   | otherwise = (makeByte . int2bin . ord) 'x'

-- Flip bit methods
flipBit :: Bit -> Bit
flipBit 0 = 1
flipBit 1 = 0

flipBits :: [Bit] -> Int -> [Bit]
flipBits [] _ = []
flipBits (x:xs) 0 = flipBit x : xs
flipBits (x:xs) n = x : flipBits xs (n - 1)

channel :: [Int] -> [Bit] -> [Bit]
channel [] ys = ys
channel [x] ys = flipBits ys x
channel (x:xs) ys = channel ((x + head xs) : tail xs) (flipBits ys x)

encode :: String -> [Bit]
encode = concat . map (addParityBit . makeByte . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int . parityCheck) . chop9bits