module BowlingKata.Day9 ( tests ) where

    import Test.Hspec

    score :: [Int] -> Int
    score ([]) = 0
    score (roll:[]) = roll
    score (roll1:roll2:[]) = roll1 + roll2
    score (roll1:roll2:roll3:[]) = roll1 + roll2 + roll3
    score (roll1:roll2:roll3:rolls) | isStrike roll1 = 10 + roll2 + roll3 + score (roll2:roll3:rolls)
                                    | isSpare roll1 roll2 = 10 + roll3 + score (roll3:rolls)
                                    | otherwise = framePoints roll1 roll2 + score (roll3:rolls)

    isSpare :: Int -> Int -> Bool
    isSpare roll1 roll2 = framePoints roll1 roll2 == 10

    isStrike :: Int -> Bool
    isStrike roll = roll == 10

    framePoints :: Int -> Int -> Int
    framePoints roll1 roll2 = roll1 + roll2

    tests = do
        it "is a gutter game"
            ((score . rollMany 20 $ 0) == 0)

        it "rolls all ones"
            ((score . rollMany 20 $ 1) == 20)

        it "rolls one spare"
            ((score $ rollSpare()++ 3:(rollMany 17 $ 0)) == 16)

        it "rolls one strike"
            ((score $ 10:4:3:(rollMany 16 $ 0)) == 24)

        it "is a perfect game"
            ((score . rollMany 12 $ 10) == 300)

    rollMany :: Int -> Int -> [Int]
    rollMany times pins = replicate times pins

    rollSpare :: () -> [Int]
    rollSpare () = [5, 5]
