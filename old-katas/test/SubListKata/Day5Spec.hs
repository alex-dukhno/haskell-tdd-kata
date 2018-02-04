module SubListKata.Day5Spec (spec) where

    import Test.Hspec
    import SubListKata.Day5 (sublist, Result(..))

    spec :: Spec
    spec = do
        it "empty equals empty" $ do
            sublist "" "" `shouldBe` Equal

        it "empty is sublist of anything" $ do
            sublist "" "anything" `shouldBe` Sublist

        it "anything is superlist of empty" $ do
            sublist "anything" "" `shouldBe` Superlist

        it "1 is not 2" $ do
            sublist "1" "2" `shouldBe` Unequal

        it "100 is 100" $ do
            sublist "100" "100" `shouldBe` Equal

        it "11 is sublist of 991199" $ do
            sublist "11" "991199" `shouldBe` Sublist

        it "991199 is superlist of 11" $ do
            sublist "991199" "11" `shouldBe` Superlist
