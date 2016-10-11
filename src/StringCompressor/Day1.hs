module StringCompressor.Day1 (tests) where

    import Test.Hspec
    import Data.Char (intToDigit)

    compress :: Maybe String -> String
    compress input = case input of
        Just ""         -> ""
        Nothing         -> ""
        Just (c:str)    -> parse 1 c str
        where
            parse counter c "" = intToDigit counter : c : ""
            parse counter previous (next:str) =
                if next == previous
                then
                    parse (counter + 1) previous str
                else
                    intToDigit counter : previous : parse 1 next str

    tests = do
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
