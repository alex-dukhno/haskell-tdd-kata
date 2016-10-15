module ScrabbleScoreKata.Day1Spec (spec) where

    import Test.Hspec
    import ScrabbleScoreKata.Day1 (score)

    spec :: Spec
    spec = do
        it "is zero when empty input" $ do
            score "" `shouldBe` 0

        it "lowercase letter" $ do
            score "a" `shouldBe` 1

        it "uppercase letter" $ do
            score "A" `shouldBe` 1

        it "valuable letter" $ do
            score "f" `shouldBe` 4

        it "short word" $ do
            score "at" `shouldBe` 2

        it "short, valuable word" $ do
            score "zoo" `shouldBe` 12

        it "medium" $ do
            score "street" `shouldBe` 6

        it "medium, valuable word" $ do
            score "quirky" `shouldBe` 22

        it "long, mixed-case word" $ do
            score "OxyphenButazone" `shouldBe` 41

        it "english-like word" $ do
            score "pinata" `shouldBe` 8

        it "non-english letter is not scored" $ do
            score "piñata" `shouldBe` 7
