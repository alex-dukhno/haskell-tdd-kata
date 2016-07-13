module RomanNumbersKata.Day2 (tests) where

    import Test.Hspec

    toRomanNumber :: Int -> String
    toRomanNumber num = hundreds' num
        where
            hundreds' :: Int -> String
            hundreds' num =
                let hundreds    = div num 100
                    rest        = mod num 100
                in case hundreds of
                    0 -> fifties' rest
                    _ -> 'C':(fifties' rest)
            fifties' :: Int -> String
            fifties' num =
                let fifties = div num 50
                    rest    = mod num 50
                in case fifties of
                    0 -> tens' rest
                    _ -> 'L':(tens' rest)
            tens' :: Int -> String
            tens' num =
                let tens    = div num 10
                    rest    = mod num 10
                in case tens of
                    0 -> fives' num
                    4 -> 'X':'L':(fives' rest)
                    _ -> 'X':(fives' rest)

            fives' :: Int -> String
            fives' num
                | num == 9  = "IX"
                | otherwise =
                    let fives   = div num 5
                        rest    = mod num 5
                    in case fives of
                        0 -> ones' rest
                        _ -> 'V':(ones' rest)

            ones' :: Int -> String
            ones' ones =
                case ones of
                    4 -> "IV"
                    _ -> replicate ones 'I'

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

        it "returns \"XLIX\" when given 49"
            (toRomanNumber 49 == "XLIX")

        it "returns \"L\" when given 50"
            (toRomanNumber 50 == "L")

        it "returns \"C\" when given 100"
            (toRomanNumber 100 == "C")

        -- it "returns \"XC\" when given 90"
        --     (toRomanNumber 90 == "XC")
