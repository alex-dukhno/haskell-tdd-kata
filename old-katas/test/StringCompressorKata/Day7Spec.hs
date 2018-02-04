module StringCompressorKata.Day7Spec (spec) where

    import Test.Hspec
    import StringCompressorKata.Day7 (compress)

    spec :: Spec
    spec = do
        it "compresses Nothing to another Nothing" $ do
            compress Nothing `shouldBe` Nothing

        it "compresses an empty string to another empty string" $ do
            compress (Just "") `shouldBe` Just ""

        it "compresses a single character string" $ do
            compress (Just "a") `shouldBe` Just "1a"

        it "compresses a string of unique characters" $ do
            compress (Just "abc") `shouldBe` Just "1a1b1c"

        it "compresses a string of doubled characters" $ do
            compress (Just "aabbcc") `shouldBe` Just "2a2b2c"
