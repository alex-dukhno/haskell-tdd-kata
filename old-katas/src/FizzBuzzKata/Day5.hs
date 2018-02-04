module FizzBuzzKata.Day5 (fizzbuzz) where

    fizzbuzz :: [Int] -> [String]
    fizzbuzz [] = []
    fizzbuzz nums = map numToFizzBuzz nums
        where
            numToFizzBuzz :: Int -> String
            numToFizzBuzz n
                | isFizz n
                    && isBuzz n = "fizz!buzz!"
                | isFizz n      = "fizz!"
                | isBuzz n      = "buzz!"
                | otherwise     = show n

            isFizz :: Int -> Bool
            isFizz num = num `isDivisibleBy` 3 || '3' `elementOf` show num

            isBuzz :: Int -> Bool
            isBuzz num = num `isDivisibleBy` 5 || '5' `elementOf` show num

            isDivisibleBy :: Int -> Int -> Bool
            isDivisibleBy num m = num `mod` m == 0

            elementOf :: Char -> String -> Bool
            elementOf c str = c `elem` str
