module RomanNumbersKata.Day6 (tests) where

    import Test.Hspec

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 50 (exceptionForBase 50)
        where
            toRomanNumber' :: Int -> Int -> Int -> String
            toRomanNumber' num base exception
                | base == 1         = ones' num
                | exception == num  = exceptionToStr exception
                | otherwise         =
                    let times   = div num base
                        rest    = mod num base
                        next    = nextBase base
                    in case times of
                        0 -> toRomanNumber' rest next (exceptionForBase next)
                        _ -> replicate times (baseToChar base) ++ toRomanNumber' rest next (exceptionForBase next)

            exceptionForBase :: Int -> Int
            exceptionForBase base
                | base == 10    = 40
                | base == 5     = 9
                | base == 1     = 4
                | otherwise     = 0

            ones' :: Int -> String
            ones' ones
                | ones == 4 = "IV"
                | otherwise = replicate ones 'I'

            exceptionToStr :: Int -> String
            exceptionToStr exception
                | exception == 40   = "XL"
                | exception == 9    = "IX"
                | exception == 4    = "IV"
                | otherwise         = ""

            nextBase :: Int -> Int
            nextBase base
                | base == 50    = 10
                | base == 10    = 5
                | otherwise     = 1

            baseToChar :: Int -> Char
            baseToChar num
                | num == 50 = 'L'
                | num == 10 = 'X'
                | num == 5  = 'V'
                | num == 1  = 'I'

    tests = do
        it "retruns an empty string when given 0"
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

        it "returns \"XXX\" when given 30"
            (toRomanNumber 30 == "XXX")

        it "returns \"L\" when given 50"
            (toRomanNumber 50 == "L")

        {- it "returns \"XLIX\" when given 49"
            (toRomanNumber 49 == "XLIX") -}
