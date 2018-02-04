module SubListKata.Day2Spec (spec) where

    import Test.Hspec
    import SubListKata.Day2 (sublist, Result(..))

    spec :: Spec
    spec = do
        it "empty equals empty" $ do
            sublist "" "" `shouldBe` Equal

        it "empty is a sublist of anything" $ do
            sublist "" "abcd" `shouldBe` Sublist

        it "anything is a superlist of empty" $ do
            sublist "abcd" "" `shouldBe` Superlist

        it "1 is not 2" $ do
            sublist "1" "2" `shouldBe` Unequal

        it "1 is 1" $ do
            sublist "1" "1" `shouldBe` Equal

        it "1 is sublist of 12" $ do
            sublist "1" "12" `shouldBe` Sublist

        it "10 is sublist of 34101" $ do
            sublist "10" "34101" `shouldBe` Sublist

        it "567880 is superlist of 78" $ do
            sublist "567880" "78" `shouldBe` Superlist

        it "recurring values unequal" $ do
            sublist "12123" "1231232321" `shouldBe` Unequal
