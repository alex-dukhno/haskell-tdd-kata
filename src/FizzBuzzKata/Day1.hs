module FizzBuzzKata.Day1 (fizzbuzz) where

    fizzbuzz :: [Int] -> [String]
    fizzbuzz [] = []
    fizzbuzz (n:ns)
        | isFizz n
            && isBuzz n = "fizz!buzz!" : fizzbuzz ns
        | isFizz n      = "fizz!" : fizzbuzz ns
        | isBuzz n      = "buzz!" : fizzbuzz ns
        | otherwise     = show n : fizzbuzz ns
        where
            isFizz :: Int -> Bool
            isFizz num = num `isDivisibleBy` 3 || elem '3' (show num)

            isBuzz :: Int -> Bool
            isBuzz num = num `isDivisibleBy` 5 || elem '5' (show num)

            isDivisibleBy :: Int -> Int -> Bool
            isDivisibleBy num m = mod num m == 0
