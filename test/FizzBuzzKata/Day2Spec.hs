module FizzBuzzKata.Day2Spec (spec) where

    import Test.Hspec
    import FizzBuzzKata.Day2 (fizzbuzz)

    spec :: Spec
    spec = do
        it "returns [] when given []"
            (fizzbuzz [] == [])

        it "returns [\"1\"] when given [1]"
            (fizzbuzz [1] == ["1"])

        it "returns [\"fizz!\"] when given [3]"
            (fizzbuzz [3] == ["fizz!"])

        it "returns [\"fizz!\"] when given [6]"
            (fizzbuzz [6] == ["fizz!"])

        it "returns [\"buzz!\"] when given [5]"
            (fizzbuzz [5] == ["buzz!"])

        it "returns [\"buzz!\"] when given [10]"
            (fizzbuzz [10] == ["buzz!"])

        it "returns [\"fizz!buzz!\"] when given [15]"
            (fizzbuzz [15] == ["fizz!buzz!"])

        it "returns [\"fizz!\"] when given [13]"
            (fizzbuzz [13] == ["fizz!"])

        it "returns [\"buzz!\"] when given [52]"
            (fizzbuzz [52] == ["buzz!"])

        it "returns [\"1\", \"fizz!\", \"buzz!\", \"fizz!buzz!\"] when given [1, 3, 5, 53]"
            (fizzbuzz [1, 3, 5, 53] == ["1", "fizz!", "buzz!", "fizz!buzz!"])
