module RomanNumbersKata.Day7 (tests) where

    import Test.Hspec

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 1000 (exceptionForBase 1000)
        where
            toRomanNumber' :: Int -> Int -> Int -> String
            toRomanNumber' num base exception =
                let next    = nextBase base
                    times   = div num base
                in case base of
                    1 -> ones' num
                    _ -> case exception of
                        0 ->
                            let rest = mod num base
                            in case times of
                                0 -> toRomanNumber' rest next (exceptionForBase next)
                                _ -> replicate times (baseToChar base) ++ toRomanNumber' rest next (exceptionForBase next)
                        _ ->
                            let exceptionTime = div num exception
                            in case exceptionTime of
                                1 -> exceptionToStr exception ++ toRomanNumber' (num - exception) next (exceptionForBase next)
                                _ ->
                                    let rest = mod num base
                                    in case times of
                                        0 -> toRomanNumber' rest next (exceptionForBase next)
                                        _ -> replicate times (baseToChar base) ++ toRomanNumber' rest next (exceptionForBase next)

            nextBase :: Int -> Int
            nextBase base
                | base == 1000  = 500
                | base == 500   = 100
                | base == 100   = 50
                | base == 50    = 10
                | base == 10    = 5
                | base == 5     = 1

            exceptionForBase :: Int -> Int
            exceptionForBase base
                | base == 1000  = 0
                | base == 500   = 900
                | base == 100   = 400
                | base == 50    = 90
                | base == 10    = 40
                | base == 5     = 9
                | base == 1     = 4

            baseToChar :: Int -> Char
            baseToChar base
                | base == 1000  = 'M'
                | base == 500   = 'D'
                | base == 100   = 'C'
                | base == 50    = 'L'
                | base == 10    = 'X'
                | base == 5     = 'V'
                | base == 1     = 'I'

            exceptionToStr :: Int -> String
            exceptionToStr exception
                | exception == 900  = "CM"
                | exception == 400  = "CD"
                | exception == 90   = "XC"
                | exception == 40   = "XL"
                | exception == 9    = "IX"
                | exception == 4    = "IV"
                | otherwise         = ""

            ones' :: Int -> String
            ones' num
                | num == 4  = "IV"
                | otherwise = replicate num 'I'

    tests = do
        it "returns an empty string when given 0"
            (toRomanNumber 0 == "")

        it "returns \"I\" when given 1"
            (toRomanNumber 1 == "I")

        it "returns \"V\" when given 5"
            (toRomanNumber 5 == "V")

        it "returns \"X\" when given 10"
            (toRomanNumber 10 == "X")

        it "returns \"IV\" when given 4"
            (toRomanNumber 4 == "IV")

        it "returns \"XIV\" when given 14"
            (toRomanNumber 14 == "XIV")

        it "returns \"IX\" when given 9"
            (toRomanNumber 9 == "IX")

        it "returns \"L\" when given 50"
            (toRomanNumber 50 == "L")

        it "returns \"XLIX\" when given 49"
            (toRomanNumber 49 == "XLIX")

        it "returns \"MMMCMXCIX\" when given 3999"
            (toRomanNumber 3999 == "MMMCMXCIX")
