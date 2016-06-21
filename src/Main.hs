module Main where

import BowlingKata.Day1

main = do

    let gutterGame = score . replicate 20 $ 0
    let allOnes = score . replicate 20 $ 1
    let oneSpare = score $ [5,5] ++ 3: (replicate 17 0)
    let oneStrike = score $ 10:3:4: (replicate 16 0)
    let perfectGame = score . replicate 12 $ 10

    print [gutterGame, allOnes, oneSpare, oneStrike, perfectGame]
