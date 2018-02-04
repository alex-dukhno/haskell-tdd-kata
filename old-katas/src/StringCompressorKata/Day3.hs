module StringCompressorKata.Day3 (compress) where

    import Test.Hspec
    import Data.Char (intToDigit)

    compress :: Maybe String -> Maybe String
    compress Nothing        = Nothing
    compress (Just "")      = Just ""
    compress (Just (c:str)) = Just $ parse 1 c str
        where
            parse counter c ""                  = intToDigit counter : c : ""
            parse counter prev (current:str)    = case theSameChar of
                True    -> parse (counter + 1) prev str
                False   -> intToDigit counter : prev : parse 1 current str
                where
                    theSameChar = prev == current
