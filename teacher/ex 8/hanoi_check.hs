module Hanoi_Check where

type Peg = String
type Move = (Peg, Peg)

data Report = Bad | Ok deriving Show
type Conf = ([Int], [Int], [Int])

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a _ c = [(a, c)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, c)] ++ hanoi (n - 1) b a c

-- Check the existence of the peg
checkPegExistence :: Peg -> Bool
checkPegExistence peg | peg == "a" || peg == "b" || peg == "c" = True
                      | otherwise = False

-- Check that the selected peg is not empty
checkFirstConstraint :: Conf -> Peg -> Bool
checkFirstConstraint (a, b, c) peg  | peg == "a" && a /= [] = True
                                    | peg == "b" && b /= [] = True
                                    | peg == "c" && c /= [] = True
                                    | otherwise = False

-- Check the selected move is legal
checkSecondConstraint :: Conf -> Move -> Bool
checkSecondConstraint (a, b, c) (from, to) | from == "a" && to == "b" && b /= [] && head a > head b = False
                                           | from == "a" && to == "c" && c /= [] && head a > head c = False
                                           | from == "b" && to == "a" && a /= [] && head b > head a = False
                                           | from == "b" && to == "c" && c /= [] && head b > head c = False
                                           | from == "c" && to == "a" && a /= [] && head c > head a = False
                                           | from == "c" && to == "b" && b /= [] && head c > head b = False
                                           | otherwise = True

-- Simulate and check the execution of hanoi algorithm
checkExecution :: Conf -> Move -> Conf
checkExecution (a, b, c) (from, to) | from == "a" && to == "b" = (tail a, head a : b, c)
                                    | from == "a" && to == "c" = (tail a, b, head a : c)
                                    | from == "b" && to == "a" = (head b : a, tail b, c)
                                    | from == "b" && to == "c" = (a, tail b, head b : c)
                                    | from == "c" && to == "a" = (head c : a, b, tail c)
                                    | from == "c" && to == "b" = (a, head c : b, tail c)
                                    | otherwise = (a, b, c)

check :: Conf -> Int -> [Move] -> (Report, Int, [Move], Conf)
check conf n [] = (Ok, n, [], conf)
check conf n ((m1, m2):ms) =  if checkPegExistence m1 && checkPegExistence m2 && checkFirstConstraint conf m1 && checkSecondConstraint conf (m1, m2)
                              then
                                check (checkExecution conf (m1, m2)) (n + 1) ms
                              else
                                (Bad, n, [(m1, m2)], conf)

main :: IO ()
main = print $ check ([1,2,3,4,5], [], []) 0 [("a", "b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b"),("a","c"),("b","c"),("b","a"),("c","a"),("b","c"),("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b"),("c","a"),("b","c"),("b","a"),("c","a"),("c","b"),("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
