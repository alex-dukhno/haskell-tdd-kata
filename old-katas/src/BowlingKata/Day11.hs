module BowlingKata.Day11 (score) where

    score :: [Int] -> Int
    score ([]) = 0
    score (last:[]) = last
    score (roll1:roll2:[]) = roll1 + roll2
    score (roll1:roll2:roll3:[]) = roll1 + roll2 + roll3
    score (roll1:roll2:roll3:rolls) | isStrike = 10 + roll2 + roll3 + score (roll2:roll3:rolls)
                                    | isSpare = framePoints + roll3 + score (roll3:rolls)
                                    | otherwise = framePoints + score (roll3:rolls)
        where
            isStrike = roll1 == 10
            framePoints = roll1 + roll2
            isSpare = framePoints == 10
