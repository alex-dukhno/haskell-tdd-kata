module StringCompressorKata.Day4Spec (spec) where

    import Test.Hspec
    import StringCompressorKata.Day4 (compress)

    spec = do
        it "compresses nothing into the nothing" $ do
            compress Nothing `shouldBe` Nothing

        it "compresses an empty string into another empty string" $ do
            compress (Just "") `shouldBe` Just ""

        it "compress an one character string" $ do
            compress (Just "a") `shouldBe` Just "1a"

        it "compress a string of unique characters" $ do
            compress (Just "abc") `shouldBe` Just "1a1b1c"

        it "compress a string of doubled characters" $ do
            compress (Just "aabbcc") `shouldBe` Just "2a2b2c"
