module BowlingKata.Day5 (score) where

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
