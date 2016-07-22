module FizzBuzzKata.Day1 (tests) where

    import Test.Hspec
    import Data.Char (intToDigit)

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

    tests = do
        it "returns an empty list when given an empty list"
            (fizzbuzz [] == [])

        it "returns [\"1\"] when given [1]"
            (fizzbuzz [1] == ["1"])

        it "returns [\"1\", \"2\"] when given [1,2]"
            (fizzbuzz [1, 2] == ["1", "2"])

        it "returns [\"fizz!\"] when given [3]"
            (fizzbuzz [3] == ["fizz!"])

        it "returns [\"fizz!\" \"buzz!\"] when given [3,5]"
            (fizzbuzz [3, 5] == ["fizz!", "buzz!"])

        it "returns [\"fizz!buzz!\"] when given [15]"
            (fizzbuzz [15] == ["fizz!buzz!"])

        it "returns [\"fizz!\"] when given [13]"
            (fizzbuzz [13] == ["fizz!"])

        it "returns [\"buzz!\"] when given [52]"
            (fizzbuzz [52] == ["buzz!"])

        it "returns [\"fizz!buzz!\"] when given [35]"
            (fizzbuzz [35] == ["fizz!buzz!"])
