module SubListKata.Day6Spec (spec) where

    import Test.Hspec
    import SubListKata.Day6 (sublist, Result(..))

    spec :: Spec
    spec = do
        it "empty equals empty" $ do
            sublist "" "" `shouldBe` Equal

        it "empty is sublist of anything" $ do
            sublist "" "anything" `shouldBe` Sublist

        it "anything is superlist of empty" $ do
            sublist "anything" "" `shouldBe` Superlist

        it "1 does not equal 2" $ do
            sublist "1" "2" `shouldBe` Unequal

        it "100 equals 100" $ do
            sublist "100" "100" `shouldBe` Equal

        it "11 is sublist of 221122" $ do
            sublist "11" "221122" `shouldBe` Sublist

        it "221122 is superlist of 11" $ do
            sublist "221122" "11" `shouldBe` Superlist
