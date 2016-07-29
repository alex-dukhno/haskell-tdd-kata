module FizzBuzzKata.Day8 (tests) where

    import Test.Hspec

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

    tests = do
        it "returns [] when given []"
            (fizzbuzz [] == [])

        it "returns [\"1\"] when given [1]"
            (fizzbuzz [1] == ["1"])

        it "returns [\"fizz!\"] when given [3]"
            (fizzbuzz [3] == ["fizz!"])

        it "returns [\"fizz!\"] when given [6]"
            (fizzbuzz [6] == ["fizz!"])

        it "returns [\"fizz!\"] when given [13]"
            (fizzbuzz [13] == ["fizz!"])

        it "returns [\"buzz!\"] when given [5]"
            (fizzbuzz [5] == ["buzz!"])

        it "returns [\"buzz!\"] when given [10]"
            (fizzbuzz [10] == ["buzz!"])

        it "returns [\"buzz!\"] when given [52]"
            (fizzbuzz [52] == ["buzz!"])

        it "returns [\"fizz!buzz!\"] when given [35]"
            (fizzbuzz [35] == ["fizz!buzz!"])

        it "returns [\"1\", \"2\", \"fizz!\", \"4\", \"buzz!\", \"fizz!\", \"7\", \"8\", \"fizz!\", \"buzz!\", \"11\", \"fizz!\", \"fizz!\", \"14\", \"fizz!buzz!\"] when given [1..15]"
            (fizzbuzz [1..15] == ["1", "2", "fizz!", "4", "buzz!", "fizz!", "7", "8", "fizz!", "buzz!", "11", "fizz!", "fizz!", "14", "fizz!buzz!"])
