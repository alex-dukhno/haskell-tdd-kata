module FizzBuzzKata.Day1Spec (spec) where

    import Test.Hspec
    import FizzBuzzKata.Day1 (fizzbuzz)

    spec :: Spec
    spec = do
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
