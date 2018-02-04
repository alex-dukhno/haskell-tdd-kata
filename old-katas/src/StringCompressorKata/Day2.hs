module StringCompressorKata.Day2 (compress) where

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
