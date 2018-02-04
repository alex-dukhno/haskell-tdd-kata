module StringCompressorKata.Day1 (compress) where

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
