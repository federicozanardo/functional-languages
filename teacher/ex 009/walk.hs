module Walk where

type Conf = (Int, Int)
type Move = (Int, Char)

move1 :: Move -> Conf -> Conf
move1 (x, y) (l, r) | y == 'L' && (l + x) < 0 = (0, r)
                    | y == 'R' && (r + x) < 0 = (l, 0)
                    | y == 'L' && abs (l + x - r) <= 4 = (l + x, r)
                    | y == 'R' && abs (r + x - l) <= 4 = (l, r + x)
                    | otherwise                        = error "troppi uccelli"

pure' :: Conf -> [Conf]
pure' (l, r)  | abs (l - r) <= 4  = []
              | otherwise         = error "troppi uccelli"

play1 :: [Move] -> (Conf -> [Conf])
play1 = foldr (\a b x -> (move1 a x) : b (move1 a x)) pure'

move2 :: Move -> Conf -> Conf
move2 (x, y) (l, r) | y == 'L' && (l + x) < 0 = (0, r)
                    | y == 'R' && (r + x) < 0 = (l, 0)
                    | y == 'L'  = (l + x, r)
                    | y == 'R'  = (l, r + x)

pure'' :: Conf -> Maybe [Conf]
pure'' (l, r)  | abs (l - r) <= 4  = Just []
              | otherwise         = Nothing

-- fmap :: ([Conf] -> [Conf]) -> Maybe [Conf] -> Maybe [Conf]
play2 :: [Move] -> (Conf -> Maybe [Conf])
play2 = foldr (\a b x -> fmap ((:) (move2 a x)) (b (move2 a x))) pure''

move3 :: Move -> Maybe Conf -> Maybe Conf
move3 _ Nothing = Nothing
move3 (x, y) (Just (l, r))  | y == 'L' && (l + x) < 0 = Just (0, r)
                            | y == 'R' && (r + x) < 0 = Just (l, 0)
                            | y == 'L' && abs (l + x - r) <= 4 = Just (l + x, r)
                            | y == 'R' && abs (r + x - l) <= 4 = Just (l, r + x)
                            | otherwise                        = Nothing

play3 :: Maybe Conf -> [Move] -> Maybe [Conf]
play3 Nothing [] = Nothing
play3 (Just s) [] = Just []
play3 s (m:ms) = pure (:) <*> (move3 m s) <*> play3 (move3 m s) ms
--               (:)          x               xs
