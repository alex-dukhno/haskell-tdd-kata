module UnclosedBracketsKata.Day1Spec (spec) where

    import Test.Hspec
    import UnclosedBracketsKata.Day1 (brackets)

    spec :: Spec
    spec = do
        it "has none unclosed bracket when given an empty string" $ do
            brackets "" `shouldBe` 0

        it "1 unclosed brackets when given '('" $ do
            brackets "(" `shouldBe` 1

        it "-1 unclosed brackets when given ')'" $ do
            brackets ")" `shouldBe` -1

        it "3 unclosed brackets when given '((('" $ do
            brackets "(((" `shouldBe` 3

        it "2 unclosed brackets when given ')((('" $ do
            brackets ")(((" `shouldBe` 2
