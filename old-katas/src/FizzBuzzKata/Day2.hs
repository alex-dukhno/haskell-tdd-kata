module FizzBuzzKata.Day2 (fizzbuzz) where

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
            isFizz num = num `isDivisible` 3 || elem '3' (show num)

            isBuzz :: Int -> Bool
            isBuzz num = num `isDivisible` 5 || elem '5' (show num)

            isDivisible :: Int -> Int -> Bool
            isDivisible num m = num `mod` m == 0
