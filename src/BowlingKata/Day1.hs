module BowlingKata.Day1 ( tests ) where

    import Test.Hspec

    score :: [Int] -> Int
    score [] = 0
    score (x:[]) = x
    score (x:y:[]) = x + y
    score (x:y:z:[]) = x + y + z
    score (x:y:z:xs)  | x == 10 = 10 + y + z + score(y:z:xs)
                      | (x + y) == 10 = x + y + z + score (z:xs)
                      | otherwise = x + y + score (z:xs)

    tests = do
        it "is a gutter game"
            ((score . replicate 20 $ 0) == 0)

        it "rolls all ones"
            ((score . replicate 20 $ 1) == 20)

        it "rolls one spare"
            ((score $ 5:5:3:(replicate 17 $ 0)) == 16)

        it "rolls one strike"
            ((score $ 10:4:3:(replicate 16 $ 0)) == 24)

        it "is a perfect game"
            ((score . replicate 12 $ 10) == 300)
