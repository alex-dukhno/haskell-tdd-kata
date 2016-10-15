module RomanNumbersKata.Day9 (toRomanNumber) where

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 50 (exceptionForBase 50)
            where
                toRomanNumber' :: Int -> Int -> Maybe Int -> String
                toRomanNumber' num base exception
                    | num == 0  = ""
                    | num == 1  = "I"
                    | otherwise =
                        let times   = div num base
                            rest    = mod num base
                            next    = nextBase base
                        in case exception of
                            Just num -> exceptionToStr exception
                            _ ->
                                case times of
                                    0 -> toRomanNumber' rest next (exceptionForBase next)
                                    _ -> replicate times (baseToChar base) ++ toRomanNumber' rest next (exceptionForBase next)

                exceptionForBase :: Int -> Maybe Int
                exceptionForBase base
                    | base == 50    = Nothing
                    | base == 10    = Just 40
                    | base == 5     = Just 9
                    | base == 1     = Just 4

                exceptionToStr :: Maybe Int -> String
                exceptionToStr exception =
                    case exception of
                        Just 40 -> "XL"
                        Just 9  -> "IX"
                        Just 4  -> "IV"
                        Nothing -> ""

                nextBase :: Int -> Int
                nextBase base
                    | base == 50    = 10
                    | base == 10    = 5
                    | base == 5     = 1

                baseToChar :: Int -> Char
                baseToChar base
                    | base == 50    = 'L'
                    | base == 10    = 'X'
                    | base == 5     = 'V'
                    | base == 1     = 'I'
