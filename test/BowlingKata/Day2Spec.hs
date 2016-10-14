module BowlingKata.Day2Spec (spec) where

    import Test.Hspec
    import BowlingKata.Day2 (score)

    spec :: Spec
    spec = do
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
