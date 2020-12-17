module Ex where
--contains the required 5 functions

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = [x] ++ init' xs

pick :: (Num a) => Int -> [a] -> a
pick _ [] = (-1)
pick 0 (x:_) = x
pick n (_:xs) = pick (n-1) xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = [x] ++ take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs