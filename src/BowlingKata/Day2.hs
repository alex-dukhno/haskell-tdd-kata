module BowlingKata.Day2 (score) where

    score :: [Int] -> Int
    score ([]) = 0
    score (x:[]) = x
    score (x:y:[]) = x + y
    score (x:y:z:[]) = x + y + z
    score (x:y:z:xs)    | x == 10 = 10 + y + z + score (y:z:xs)
                        | (x + y) == 10 = 10 + z + score (z:xs)
                        | otherwise = x + y + score (z:xs)
