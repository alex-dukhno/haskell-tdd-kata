module BowlingKata.Day5 (tests) where

    import Test.Hspec

    score :: [Int] -> Int
    score ([]) = 0
    score (lastRoll:[]) = lastRoll
    score (roll1:roll2:[]) = roll1 + roll2
    score (roll1:roll2:roll3:[]) = roll1 + roll2 + roll3
    score (roll1:roll2:roll3:rolls) = do
        if isStrike roll1
            then 10 + roll2 + roll3 + score (roll2:roll3:rolls)
            else if isSpare roll1 roll2
                then 10 + roll3 + score (roll3:rolls)
                else roll1 + roll2 + score (roll3:rolls)

    isStrike :: Int -> Bool
    isStrike roll = roll == 10

    isSpare :: Int -> Int -> Bool
    isSpare roll1 roll2 = roll1 + roll2 == 10

    tests = do
        it "is a gutter game"
            ((score . replicate 20 $ 0) == 0)

        it "rolls all ones"
            ((score . replicate 20 $ 1) == 20)

        it "rolls one spare"
            ((score $ 5:5:3:(replicate 17 $ 0)) == 16)

        it "rolls one strike"
            ((score $ 10:4:3:(replicate 16 $ 0)) == 24)

        it "is a perfect game"
            ((score . replicate 12 $ 10) == 300)
