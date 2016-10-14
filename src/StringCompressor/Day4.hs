module StringCompressor.Day4 (tests) where

    import Test.Hspec
    import Data.Char (intToDigit)

    compress :: Maybe String -> Maybe String
    compress Nothing        = Nothing
    compress (Just "")      = Just ""
    compress (Just (c:str)) = Just $ compress' 1 c str
        where
            compress' :: Int -> Char -> String -> String
            compress' counter prev ""               = intToDigit counter : prev : ""
            compress' counter prev (current:str)    = case theSameChar of
                True    -> compress' (counter + 1) prev str
                False   -> intToDigit counter : prev : compress' 1 current str
                where
                    theSameChar = prev == current

    tests = do
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
