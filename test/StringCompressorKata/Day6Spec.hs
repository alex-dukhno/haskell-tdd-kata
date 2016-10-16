module StringCompressorKata.Day6Spec (spec) where

    import Test.Hspec
    import StringCompressorKata.Day6 (compress)

    spec :: Spec
    spec = do
        it "compresses nothing into the nothing" $ do
            compress Nothing `shouldBe` Nothing

        it "compresses an empty string into another empty string" $ do
            compress (Just "") `shouldBe` Just ""

        it "compresses a single character string" $ do
            compress (Just "a") `shouldBe` Just "1a"

        it "compresses a string of unique chatacters" $ do
            compress (Just "abc") `shouldBe` Just "1a1b1c"

        it "compresses a string of doubled characters" $ do
            compress (Just "aabbcc") `shouldBe` Just "2a2b2c"
