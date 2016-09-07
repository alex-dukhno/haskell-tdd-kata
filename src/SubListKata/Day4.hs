module SubListKata.Day4 (tests) where

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
        it "empty equals empty" $ do
            sublist "" "" `shouldBe` Equal

        it "empty is sublinst of anything" $ do
            sublist "" "anything" `shouldBe` Sublist

        it "anything is superlist of empty" $ do
            sublist "anything" "" `shouldBe` Superlist

        it "1 does not equal 2" $ do
            sublist "1" "2" `shouldBe` Unequal

        it "100 equals to 100" $ do
            sublist "100" "100" `shouldBe` Equal

        it "11 is sublist of 44311234" $ do
            sublist "11" "44311234" `shouldBe` Sublist

        it "6545590 is superlist of 55" $ do
            sublist "6545590" "55" `shouldBe` Superlist
