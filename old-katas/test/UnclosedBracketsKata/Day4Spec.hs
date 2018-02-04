module UnclosedBracketsKata.Day4Spec (spec) where

    import Test.Hspec
    import UnclosedBracketsKata.Day4

    spec :: Spec
    spec = do
        it "has none unclosed brackets when given an empty string" $ do
            brackets "" `shouldBe` 0

        it "has one unclosed bracket when given a '(' string" $ do
            brackets "(" `shouldBe` 1

        it "has three unclosed brackets when given '((('" $ do
            brackets "(((" `shouldBe` 3

        it "has two unclosed brackets when given ')((('" $ do
            brackets ")(((" `shouldBe` 2
