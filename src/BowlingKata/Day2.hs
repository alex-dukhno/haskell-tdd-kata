module BowlingKata.Day2 where

score :: [Int] -> Int
score ([]) = 0
score (x:[]) = x
score (x:y:[]) = x + y
score (x:y:z:[]) = x + y + z
score (x:y:z:xs)    | x == 10 = 10 + y + z + score (y:z:xs)
                    | (x + y) == 10 = 10 + z + score (z:xs)
                    | otherwise = x + y + score (z:xs)

assert name actual expected = do
    if actual == expected
        then print $ name ++ " PASSED"
        else print $ name ++ " FAILED expected <" ++ show expected ++ "> but was <" ++ show actual ++ ">"

gutterGame = do
    assert "gutter game" (score . replicate 20 $ 0) 0


allOnes = do
    assert "all ones" (score . replicate 20 $ 1) 20

oneSpare = do
    assert "one spare" (score $ 5:5:3:(replicate 17 $ 0)) 16

oneStrike = do
    assert "one strike" (score $ 10:4:3:(replicate 16 $ 0)) 24

perfectGame = do
    assert "perfect game" (score . replicate 12 $ 10) 300