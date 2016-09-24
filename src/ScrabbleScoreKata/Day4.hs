module ScrabbleScoreKata.Day4 (tests) where

    import Test.Hspec
    import Data.Char (toLower)

    letters = [('a', 1), ('t', 1), ('o', 1), ('s', 1), ('r', 1), ('e', 1), ('u', 1), ('i', 1), ('n', 1)
                , ('p', 3), ('b', 3)
                , ('f', 4), ('y', 4), ('h', 4)
                , ('k', 5)
                , ('x', 8)
                , ('z', 10), ('q', 10)]

    score :: String -> Int
    score []        = 0
    score (c:src)   = letterValue (toLower c) + score src
        where
            letterValue :: Char -> Int
            letterValue c = case lookup c letters of
                Just v  -> v
                Nothing -> 0

    tests = do
        it "is zero when empty input" $ do
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
