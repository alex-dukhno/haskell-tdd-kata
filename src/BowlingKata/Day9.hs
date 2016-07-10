module BowlingKata.Day9 ( tests ) where

    import Test.HUnit

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

    tests = TestList
        [ TestLabel "gutter game" gutterGame
        , TestLabel "all ones" allOnes
        , TestLabel "one spare" oneSpare
        , TestLabel "one strike" oneStrike
        , TestLabel "perfect game" perfectGame
        ]

    gutterGame = TestCase ( assertEqual "gutter game" 0 (score . rollMany 20 $ 0))
    allOnes = TestCase ( assertEqual "all ones" 20 (score . rollMany 20 $ 1))
    oneSpare = TestCase ( assertEqual "one spare" 16 ( score $ rollSpare () ++ 3:(rollMany 17 0)))
    oneStrike = TestCase ( assertEqual "one strike" 24 ( score $ 10:4:3:(rollMany 16 0)))
    perfectGame = TestCase ( assertEqual "perfect game" 300 ( score . rollMany 12 $ 10))

    rollMany :: Int -> Int -> [Int]
    rollMany times pins = replicate times pins

    rollSpare :: () -> [Int]
    rollSpare () = [5, 5]
