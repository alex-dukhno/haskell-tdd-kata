module StringCompressorKata.Day1Spec (spec) where

    import Test.Hspec
    import StringCompressorKata.Day1 (compress)

    spec :: Spec
    spec = do
        it "compresses nothing to empty string" $ do
            compress Nothing `shouldBe` ""

        it "compresses empty string to empty string" $ do
            compress (Just "") `shouldBe` ""

        it "compresses one char string" $ do
            compress (Just "a") `shouldBe` "1a"

        it "compresses string of unique characters" $ do
            compress (Just "abc") `shouldBe` "1a1b1c"

        it "compress string of repeated characters" $ do
            compress (Just "aabbcc") `shouldBe` "2a2b2c"
