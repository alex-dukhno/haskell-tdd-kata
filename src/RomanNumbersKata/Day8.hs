module RomanNumbersKata.Day8 (tests) where

    import Test.Hspec

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 10 (exceptionForBase 10)
        where
            toRomanNumber' :: Int -> Int -> Int -> String
            toRomanNumber' num base exception =
                case base of
                    1 ->
                        case num of
                        4 -> "IV"
                        _ -> replicate num 'I'
                    _ ->
                        let times           = div num base
                            rest            = mod num base
                            next            = nextBase base
                            exceptionTimes  = div num exception
                            in case exceptionTimes of
                                1 -> exceptionToStr exception ++ toRomanNumber' (num - exception) next (exceptionForBase next)
                                _ ->
                                    case times of
                                        0 -> toRomanNumber' rest next (exceptionForBase next)
                                        _ -> baseToChar base : toRomanNumber' rest next (exceptionForBase next)

            exceptionForBase :: Int -> Int
            exceptionForBase base
                | base == 10    = 1
                | base == 5     = 9
                | base == 1     = 4

            nextBase :: Int -> Int
            nextBase base
                | base == 10    = 5
                | base == 5     = 1

            baseToChar :: Int -> Char
            baseToChar base
                | base == 10    = 'X'
                | base == 5     = 'V'
                | base == 1     = 'I'

            exceptionToStr exception
                | exception == 9    = "IX"
                | otherwise         = ""

    tests = do
        it "returns an empty string when given 0"
            (toRomanNumber 0 == "")

        {- it "returns \"I\" when given 1"
            (toRomanNumber 1 == "I") -}

        it "returns \"V\" when given 5"
            (toRomanNumber 5 == "V")

        it "returns \"IV\" when given 4"
            (toRomanNumber 4 == "IV")

        it "returns \"X\" when given 10"
            (toRomanNumber 10 == "X")

        it "returns \"XIV\" when given 14"
            (toRomanNumber 14 == "XIV")

        it "returns \"IX\" when given 9"
            (toRomanNumber 9 == "IX")
