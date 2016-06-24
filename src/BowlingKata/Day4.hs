module BowlingKata.Day4 (gutterGame, allOnes, oneSpare, oneStrike, perfectGame) where

import Assert

score :: [Int] -> Int
score ([]) = 0
score (roll:[]) = roll
score (roll1:roll2:[]) = roll1 + roll2
score (roll1:roll2:roll3:[]) = roll1 + roll2 + roll3
score (roll1:roll2:roll3:rolls) | roll1 == 10 = 10 + roll2 + roll3 + score (roll2:roll3:rolls)
                                | roll1 + roll2 == 10 = 10 + roll3 + score (roll3:rolls)
                                | otherwise = roll1 + roll2 + score (roll3:rolls)

gutterGame = assert "gutter game" (score . replicate 20 $ 0) 0

allOnes = assert "all ones" (score . replicate 20 $ 1) 20

oneSpare = assert "one spare" (score (5:5:3:(replicate 17 $ 0))) 16

oneStrike = assert "one strike" (score (10:4:3:(replicate 16 $ 0))) 24

perfectGame = assert "perfect game" (score . replicate 12 $ 10) 300
