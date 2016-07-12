module RomanNumbersKata.Day1 (tests) where

    import Test.Hspec

    toRomanNumber :: Int -> String
    toRomanNumber num = tenth' num
        where
            tenth' :: Int -> String
            tenth' num =
                let tens = num `div` 10
                    rest = num `mod` 10
                in case tens of
                    4 -> 'X':'L':fifth' rest
                    otherwise -> (replicate tens 'X') ++ (fifth' rest)

            fifth' :: Int -> String
            fifth' num
                | num == 9 = "IX"
                | otherwise =
                    let fifth = num `div` 5
                        rest  = num `mod` 5
                    in
                        case fifth of
                            1 -> 'V': ones' rest
                            otherwise -> ones' rest

            ones' :: Int -> String
            ones' ones
                | ones == 4  = "IV"
                | otherwise = replicate ones 'I'

    tests = do
        it "returns an empty string when given 0"
            (toRomanNumber 0 == "")

        it "returns \"I\" when given 1"
            (toRomanNumber 1 == "I")

        it "returns \"V\" when given 5"
            (toRomanNumber 5 == "V")

        it "returns \"X\" when given 10"
            (toRomanNumber 10 == "X")

        it "returns \"XV\" when given 15"
            (toRomanNumber 15 == "XV")

        it "returns \"IV\" when given 4"
            (toRomanNumber 4 == "IV")

        it "returns \"IX\" when given 9"
            (toRomanNumber 9 == "IX")

        it "returns \"XLIX\" when given 49"
            (toRomanNumber 49 == "XLIX")
