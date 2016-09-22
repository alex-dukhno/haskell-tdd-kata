module ScrabbleScoreKata.Day2 (tests) where

    import Test.Hspec
    import Data.Char (toLower)

    letters = [('a', 1), ('t', 1), ('o', 1), ('s', 1), ('e', 1), ('r', 1), ('i', 1), ('u', 1), ('n', 1)
                , ('p', 3), ('b', 3)
                , ('f', 4), ('y', 4), ('h', 4)
                , ('k', 5)
                , ('x', 8)
                , ('z', 10), ('q', 10)]

    score :: String -> Int
    score []        = 0
    score (c:str)   = (letterValue $ toLower c) + score str
        where
            letterValue :: Char -> Int
            letterValue c = case lookup c letters of
                Just (v)    -> v
                Nothing     -> 0

    tests = do
        it "is zerp when empty input" $ do
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
