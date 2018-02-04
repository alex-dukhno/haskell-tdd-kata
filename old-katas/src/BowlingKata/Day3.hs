module BowlingKata.Day3 (score) where

    score :: [Int] -> Int
    score ([]) = 0
    score (x:[]) = x
    score (x:y:[]) = x + y
    score (x:y:z:[]) = x + y + z
    score (x:y:z:rolls) | x == 10 = 10 + y + z + score (y:z:rolls)
                        | (x + y) == 10 = 10 + z + score (z:rolls)
                        | otherwise = x + y + score (z:rolls)
