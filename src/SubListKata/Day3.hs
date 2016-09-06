module SubListKata.Day3 (tests) where

    import Test.Hspec
    import Data.List (isInfixOf)

    data Result = Equal | Unequal | Sublist | Superlist
        deriving (Eq, Show)

    sublist :: (Ord a) => [a] -> [a] -> Result
    sublist [] []   = Equal
    sublist [] _    = Sublist
    sublist _ []    = Superlist
    sublist l1 l2
        | l1 == l2  = Equal
        | isInfixOf l1 l2 = Sublist
        | isInfixOf l2 l1 = Superlist
        | otherwise = Unequal

    tests = do
        it "empty equals to empty" $ do
            sublist "" "" `shouldBe` Equal

        it "empty is sublist of anything" $ do
            sublist "" "abcd" `shouldBe` Sublist

        it "anything is superlist of empty" $ do
            sublist "abcd" "" `shouldBe` Superlist

        it "111 equals 111" $ do
            sublist "111" "111" `shouldBe` Equal

        it "111 not equals 222" $ do
            sublist "111" "222" `shouldBe` Unequal

        it "11 is sublist of 21113" $ do
            sublist "11" "21113" `shouldBe` Sublist

        it "566678 is superlist of 66" $ do
            sublist "566678" "66" `shouldBe` Superlist
