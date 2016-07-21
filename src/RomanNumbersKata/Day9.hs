module RomanNumbersKata.Day9 (tests) where

    import Test.Hspec

    toRomainNumber :: Int -> String
    toRomainNumber num = toRomainNumber' num 50 (exceptionForBase 50)
            where
                toRomainNumber' :: Int -> Int -> Maybe Int -> String
                toRomainNumber' num base exception
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
                                    0 -> toRomainNumber' rest next (exceptionForBase next)
                                    _ -> replicate times (baseToChar base) ++ toRomainNumber' rest next (exceptionForBase next)

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

    tests = do
        it "returns an empty string when given 0"
            (toRomainNumber 0 == "")

        it "returns \"I\" when given 1"
            (toRomainNumber 1 == "I")

        {-it "returns \"V\" when given 5"
            (toRomainNumber 5 == "V")

        it "returns \"IV\" when given 4"
            (toRomainNumber 4 == "IV")

        it "returns \"X\" when given 10"
            (toRomainNumber 10 == "X")

        it "returns \"XIV\" when given 14"
            (toRomainNumber 14 == "XIV") -}

        it "returns \"L\" when given 50"
            (toRomainNumber 50 == "L")

        {-it "returns \"XLIX\" when given 49"
            (toRomainNumber 49 == "XLIX") -}
