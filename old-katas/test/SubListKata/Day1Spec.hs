module SubListKata.Day1Spec (spec) where

    import Test.Hspec
    import SubListKata.Day1 (sublist, Result(..))

    spec :: Spec
    spec = do
        it "is equal when both empty" $ do
            sublist "" "" `shouldBe` Equal

        it "empty is sublist of anything" $ do
            sublist "" "abcd" `shouldBe` SubList

        it "anything is superlist of empty list" $ do
            sublist "abcd" "" `shouldBe` SuperList

        it "1 is not equal to 2" $ do
            sublist "1" "2" `shouldBe` Unequal

        it "compares large equal lists" $ do
            let xs = replicate 1000 'x'
            sublist xs xs `shouldBe` Equal

        -- it "sublist at start" $ do
            -- sublist "123" "12345" `shouldBe` Sublist
