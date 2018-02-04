module UnclosedBracketsKata.Day2Spec (spec) where

    import Test.Hspec
    import UnclosedBracketsKata.Day2

    spec :: Spec
    spec = do
        it "has no unclosed bracketes when given an empty string" $ do
            brackets "" `shouldBe` 0

        it "has 1 unclosed brackets when given '('" $ do
            brackets "(" `shouldBe` 1

        it "has 3 unclosed brackets when given '((('" $ do
            brackets "(((" `shouldBe` 3

        it "has 2 unclosed brackets when given '((()'" $ do
            brackets "((()" `shouldBe` 2
