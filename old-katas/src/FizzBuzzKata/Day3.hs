module FizzBuzzKata.Day3 (fizzbuzz) where

    fizzbuzz :: [Int] -> [String]
    fizzbuzz [] = []
    fizzbuzz (n:ns)
        | isFizz n
            && isBuzz n = "fizz!buzz!" : fizzbuzz ns
        | isFizz n  = "fizz!" : fizzbuzz ns
        | isBuzz n  = "buzz!" : fizzbuzz ns
        | otherwise = show n : fizzbuzz ns
        where
            isFizz :: Int -> Bool
            isFizz num = num `mod` 3 == 0 || '3' `elem` show num

            isBuzz :: Int -> Bool
            isBuzz num = num `mod` 5 == 0 || '5' `elem` show num
