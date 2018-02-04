module UnclosedBracketsKata.Day5Spec (spec) where

    import Test.Hspec
    import UnclosedBracketsKata.Day5 (brackets)

    spec :: Spec
    spec = do
        it "has 0 unclosed brackets when given an empty string" $ do
            brackets "" `shouldBe` 0

        it "has 1 unclosed brackets when given '('" $ do
            brackets "(" `shouldBe` 1

        it "has 3 unclosed brackets when given '((('" $ do
            brackets "(((" `shouldBe` 3

        it "has 2 unclosed brackets when given ')((('" $ do
            brackets ")(((" `shouldBe` 2
