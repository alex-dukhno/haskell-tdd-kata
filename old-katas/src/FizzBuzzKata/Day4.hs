module FizzBuzzKata.Day4 (fizzbuzz) where

    fizzbuzz :: [Int] -> [String]
    fizzbuzz [] = []
    fizzbuzz (n:nums)
        | isFizz n
            && isBuzz n = "fizz!buzz!" : fizzbuzz nums
        | isFizz n      = "fizz!" : fizzbuzz nums
        | isBuzz n      = "buzz!" : fizzbuzz nums
        | otherwise     = show n : fizzbuzz nums
        where
            isFizz :: Int -> Bool
            isFizz num = num `isDivisibleBy` 3 || '3' `elementOf` show num

            isBuzz :: Int -> Bool
            isBuzz num = num `isDivisibleBy` 5 || '5' `elementOf` show num

            isDivisibleBy :: Int -> Int -> Bool
            isDivisibleBy num m = num `mod` m == 0

            elementOf :: Char -> String -> Bool
            elementOf c str = c `elem` str
