module ScrabbleScoreKata.Day10Spec (spec) where

    import Test.Hspec
    import ScrabbleScoreKata.Day10 (score)

    spec :: Spec
    spec = do
        it "is zero when given an empty input" $ do
            score "" `shouldBe` 0

        it "is 1 when given lowercase 'a'" $ do
            score "a" `shouldBe` 1

        it "is 1 when given uppercase 'A'" $ do
            score "A" `shouldBe` 1

        it "is 4 when given 'f'" $ do
            score "f" `shouldBe` 4

        it "is 2 when given the word 'at'" $ do
            score "at" `shouldBe` 2

        it "is 12 when given the word 'zoo'" $ do
            score "zoo" `shouldBe` 12

        it "is 6 when given the word 'street'" $ do
            score "street" `shouldBe` 6

        it "is 22 when given the word 'quirky'" $ do
            score "quirky" `shouldBe` 22

        it "is 41 when given the word 'OxyphenButazone'" $ do
            score "OxyphenButazone" `shouldBe` 41

        it "scores only english-like letters" $ do
            score "pinata" `shouldBe` 8
            score "piñata" `shouldBe` 7
