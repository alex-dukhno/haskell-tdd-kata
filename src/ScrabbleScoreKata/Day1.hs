module ScrabbleScoreKata.Day1 (tests) where

    import Test.Hspec
    import Data.Char (toLower)

    score :: String -> Int
    score input = foldl (+) 0 src
        where
            src = map letterValue input
            letterValue :: Char -> Int
            letterValue l = case lookup letter letters of
                Just v  -> v
                Nothing -> 0
                where letter = toLower l

            letters = [('a', 1), ('t', 1) , ('o', 1), ('s', 1), ('r', 1), ('e', 1),('u', 1), ('i', 1), ('n', 1)
                        , ('p', 3), ('b', 3)
                        , ('f', 4), ('y', 4), ('h', 4)
                        , ('k', 5)
                        , ('x', 8)
                        , ('z', 10), ('q', 10)]

    tests = do
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
