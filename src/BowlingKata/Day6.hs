module BowlingKata.Day6 (gutterGame, allOnes, oneSpare, oneStrike, perfectGame) where

    import Assert

    score :: [Int] -> Int
    score ([]) = 0
    score (x:[]) = x
    score (x:y:[]) = x + y
    score (x:y:z:[]) = x + y + z
    score (x:y:z:xs)    | x == 10 = 10 + y + z + score (y:z:xs)
                        | (x + y) == 10 = 10 + z + score (z:xs)
                        | otherwise = x + y + score (z:xs)

    gutterGame = assert "gutter game" (score . replicate 20 $ 0) 0
    allOnes = assert "all ones" (score .replicate 20 $ 1) 20
    oneSpare = assert "one spare" (score (5:5:3:(replicate 17 $ 0))) 16
    oneStrike = assert "one strike" (score (10:4:3:(replicate 16 $ 0))) 24
    perfectGame = assert "perfect game" (score . replicate 12 $ 10) 300
