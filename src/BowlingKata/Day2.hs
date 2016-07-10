module BowlingKata.Day2 (tests) where

import Test.HUnit

score :: [Int] -> Int
score ([]) = 0
score (x:[]) = x
score (x:y:[]) = x + y
score (x:y:z:[]) = x + y + z
score (x:y:z:xs)    | x == 10 = 10 + y + z + score (y:z:xs)
                    | (x + y) == 10 = 10 + z + score (z:xs)
                    | otherwise = x + y + score (z:xs)

tests = TestList
    [ TestLabel "GutterGame" gutterGame
    , TestLabel "AllOnes" allOnes
    , TestLabel "OneSpare" oneSpare
    , TestLabel "OneStrike" oneStrike
    , TestLabel "PerfectGame" perfectGame
    ]

gutterGame = do
    TestCase (assertEqual "gutter game" (score . replicate 20 $ 0) 0)

allOnes = do
    TestCase (assertEqual "all ones" (score . replicate 20 $ 1) 20)

oneSpare = do
    TestCase(assertEqual "one spare" (score $ 5:5:3:(replicate 17 $ 0)) 16)

oneStrike = do
    TestCase(assertEqual "one strike" (score $ 10:4:3:(replicate 16 $ 0)) 24)

perfectGame = do
    TestCase(assertEqual "perfect game" (score . replicate 12 $ 10) 300)
