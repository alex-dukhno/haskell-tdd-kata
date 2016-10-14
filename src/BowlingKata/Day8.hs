module BowlingKata.Day8 (score) where

    score :: [Int] -> Int
    score ([]) = 0
    score (roll:[]) = roll
    score (roll1:roll2:[]) = roll1 + roll2
    score (roll1:roll2:roll3:[]) = roll1 + roll2 + roll3
    score (roll1:roll2:roll3:rolls) | isStrike roll1 = 10 + roll2 + roll3 + score (roll2:roll3:rolls)
                                    | isSpare roll1 roll2 = 10 + roll3 + score (roll3:rolls)
                                    | otherwise = roll1 + roll2 + score (roll3:rolls)

    isSpare :: Int -> Int -> Bool
    isSpare roll1 roll2 = roll1 + roll2 == 10

    isStrike :: Int -> Bool
    isStrike roll = roll == 10
