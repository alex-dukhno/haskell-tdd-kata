module Main where

import BowlingKata.Day1
import BowlingKata.Day2

bowlingDay1 = do
    let gutterGame = BowlingKata.Day1.score . replicate 20 $ 0
    let allOnes = BowlingKata.Day1.score . replicate 20 $ 1
    let oneSpare = BowlingKata.Day1.score $ [5,5] ++ 3: (replicate 17 0)
    let oneStrike = BowlingKata.Day1.score $ 10:3:4: (replicate 16 0)
    let perfectGame = BowlingKata.Day1.score . replicate 12 $ 10
    print [gutterGame, allOnes, oneSpare, oneStrike, perfectGame]


main = do
    print "Bowling kata first day"
    bowlingDay1
    print ""
    print "Bowling kata second day"
    BowlingKata.Day2.gutterGame
    BowlingKata.Day2.allOnes
    BowlingKata.Day2.oneSpare
    BowlingKata.Day2.oneStrike
    BowlingKata.Day2.perfectGame
    print ""
