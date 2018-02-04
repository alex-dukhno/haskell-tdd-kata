module StringCompressorKata.Day3Spec (spec) where

    import Test.Hspec
    import StringCompressorKata.Day3 (compress)

    spec :: Spec
    spec = do
        it "compress nothing into nothing" $ do
            compress Nothing `shouldBe` Nothing

        it "compress an empty string into another empty string" $ do
            compress (Just "") `shouldBe` Just ""

        it "compress a one character string" $ do
            compress (Just "a") `shouldBe` Just "1a"

        it "compress a string of unique characters" $ do
            compress (Just "abc") `shouldBe` Just "1a1b1c"

        it "compress a string of doubled characters" $ do
            compress (Just "aabbcc") `shouldBe` Just "2a2b2c"
