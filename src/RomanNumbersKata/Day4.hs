module RomanNumbersKata.Day4 (tests) where

    import Test.Hspec

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 50 (exceptionForBase 50)
        where
            toRomanNumber' :: Int -> Int -> Int -> String
            toRomanNumber' num base except =
                let next    = nextBase base
                    times   = div num base
                in
                    if base == 1
                        then ones' num
                        else case except of
                            0 ->
                                let rest = mod num base
                                in case times of
                                    0 -> toRomanNumber' rest next (exceptionForBase next)
                                    _ -> baseToChar base : toRomanNumber' rest next (exceptionForBase next)
                            _ ->
                                let exceptTimes = div num except
                                in case exceptTimes of
                                    1 -> exceptionToStr except ++ toRomanNumber' (num - except) next (exceptionForBase next)
                                    _ ->
                                        let rest = mod num base
                                        in case times of
                                            0 -> toRomanNumber' rest next (exceptionForBase next)
                                            _ -> baseToChar base : toRomanNumber' rest next (exceptionForBase next)

            nextBase :: Int -> Int
            nextBase base
                | base == 50    = 10
                | base == 10    = 5
                | base == 5     = 1

            exceptionForBase :: Int -> Int
            exceptionForBase base
                | base == 50    = 0
                | base == 10    = 40
                | base == 5     = 9
                | base == 1     = 4

            baseToChar :: Int -> Char
            baseToChar base
                | base == 50    = 'L'
                | base == 10    = 'X'
                | base == 5     = 'V'
                | base == 1     = 'I'

            exceptionToStr :: Int -> String
            exceptionToStr except
                | except == 40  = "XL"
                | except == 9   = "IX"
                | except == 0   = ""

            ones' :: Int -> String
            ones' num =
                case num of
                    4 -> "IV"
                    _ -> replicate num 'I'

    tests = do
        it "returns an empty string when given 0"
            (toRomanNumber 0 == "")

        it "returns \"I\" when given 1"
            (toRomanNumber 1 == "I")

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

        it "returns \"L\" when given 50"
            (toRomanNumber 50 == "L")

        it "returns \"XLIX\" when given 49"
            (toRomanNumber 49 == "XLIX")
