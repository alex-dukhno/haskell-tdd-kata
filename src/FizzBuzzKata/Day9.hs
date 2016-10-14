module FizzBuzzKata.Day9 (fizzbuzz) where

    fizzbuzz :: [Int] -> [String]
    fizzbuzz [] = []
    fizzbuzz nums = map toFizzBuzz nums
        where
            toFizzBuzz :: Int -> String
            toFizzBuzz n
                | isFizz n
                    && isBuzz n = "fizz!buzz!"
                | isFizz n = "fizz!"
                | isBuzz n = "buzz!"
                | otherwise = show n

            isFizz :: Int -> Bool
            isFizz num = num `isDivisibleBy` 3 || '3' `elem` show num

            isBuzz :: Int -> Bool
            isBuzz num = num `isDivisibleBy` 5 || '5' `elem` show num

            isDivisibleBy :: Int -> Int -> Bool
            isDivisibleBy num m = num `mod` m == 0
