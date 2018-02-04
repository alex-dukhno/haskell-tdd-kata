module FizzBuzzKata.Day8 (fizzbuzz) where

    fizzbuzz :: [Int] -> [String]
    fizzbuzz [] = []
    fizzbuzz nums = map toFizzBuzz nums
        where
            toFizzBuzz :: Int -> String
            toFizzBuzz num
                | isFizz num
                    && isBuzz num   = "fizz!buzz!"
                | isFizz num        = "fizz!"
                | isBuzz num        = "buzz!"
                | otherwise         = show num

            isFizz :: Int -> Bool
            isFizz num = num `isDivisibleBy` 3 || '3' `elem` show num

            isBuzz :: Int -> Bool
            isBuzz num = num `isDivisibleBy` 5 || '5' `elem` show num

            isDivisibleBy :: Int -> Int -> Bool
            isDivisibleBy num m = num `mod` m == 0
