module BowlingGame.KataSpec (spec) where

    import Test.Hspec
    import BowlingGame.Kata (score)

    spec :: Spec
    spec =
        describe "Bowling Game" $ do
            it "processes gutter game" $
                (score (rollMany 20 0) `shouldBe` 0)

            it "processes all ones" $
                (score (rollMany 20 1) `shouldBe` 20)

            it "processes one spare"
                (score (rollSpare ++ 3:rollMany 17 0) `shouldBe` 16)

            it "processes one strike"
                (score (rollStrike ++ 4:3:rollMany 16 0) `shouldBe` 24)

            it "processes perfect game"
                (score (rollMany 12 10) `shouldBe` 300)

    rollMany :: Int -> Int -> [Int]
    rollMany = replicate

    rollSpare :: [Int]
    rollSpare = [4, 6]

    rollStrike :: [Int]
    rollStrike = [10]
