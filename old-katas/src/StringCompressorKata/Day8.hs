module StringCompressorKata.Day8 (compress) where

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
                    theSameChar = current == prev
