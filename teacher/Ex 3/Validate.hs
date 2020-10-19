module Validate where

convert :: (Integral a) => a -> [a]
convert 0 = []
convert n = n `rem` 10 : convert (n `div` 10)

double :: (Num a, Ord a) => [a] -> [a]
double [] = []
double [x] = [x]
double (x:y:ys) = if y * 2 >= 9
                   then x:((y * 2) - 9) : double ys
                   else x:(y * 2) : double ys

validate :: (Integral a) => a -> Bool
validate n = sum (double (convert n)) `rem` 10 == 0