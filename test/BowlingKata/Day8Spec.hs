module BowlingKata.Day8Spec (spec) where

    import Test.Hspec
    import BowlingKata.Day8 (score)

    spec :: Spec
    spec = do
        it "is a gutter game"
            ((score . rollMany 20 $ 0) == 0)

        it "rolls all ones"
            ((score . rollMany 20 $ 1) == 20)

        it "rolls one spare"
            ((score $ rollSpare()++3:(rollMany 17 $ 0)) == 16)

        it "rolls one strike"
            ((score $ rollStrike():4:3:(rollMany 16 $ 0)) == 24)

        it "is a perfect game"
            ((score . rollMany 12 $ 10) == 300)

    rollMany :: Int -> Int -> [Int]
    rollMany times pins = replicate times pins

    rollSpare :: () -> [Int]
    rollSpare () = [5, 5]

    rollStrike :: () -> Int
    rollStrike () = 10

    isStrike :: Int -> Bool
    isStrike roll = roll == 10

    isSpare :: Int -> Int -> Bool
    isSpare roll1 roll2 = roll1 + roll2 == 10
