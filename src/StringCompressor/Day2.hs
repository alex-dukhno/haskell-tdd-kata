module StringCompressor.Day2 (tests) where

    import Test.Hspec
    import Data.Char (intToDigit)

    compress :: Maybe String -> Maybe String
    compress Nothing        = Nothing
    compress (Just "")      = Just ""
    compress (Just (c:str)) = Just (parse 1 c str)
        where
            parse :: Int -> Char -> String -> String
            parse counter c ""                  = intToDigit counter : c : ""
            parse counter prev (current:str)    = let same = prev == current in case same of
                True    -> parse (counter + 1) prev str
                False   -> intToDigit counter : prev : parse 1 current str

    tests = do
        it "compress nothing into nothing" $ do
            compress Nothing `shouldBe` Nothing

        it "compress an empty string into another empty string" $ do
            compress (Just "") `shouldBe` (Just "")

        it "compress a single character string" $ do
            compress (Just "a") `shouldBe` (Just "1a")

        it "compress a string with unique characters" $ do
            compress (Just "abc") `shouldBe` (Just "1a1b1c")

        it "compress a string with doubled characters" $ do
            compress (Just "aabbcc") `shouldBe` (Just "2a2b2c")
