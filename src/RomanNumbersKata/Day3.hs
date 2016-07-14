module RomanNumbersKata.Day3 (tests) where

    import Test.Hspec

    toRomanNumber :: Int -> String
    toRomanNumber num
        | num == 0  = ""
        | otherwise = toRomanNumber' num 10 (-1)
        where
            toRomanNumber' :: Int -> Int -> Int -> String
            toRomanNumber' num base except
                | base == 1     = ones' num
                | num == except = exceptToString except
                | otherwise     =

                    let times   = div num base
                        rest    = mod num base
                    in case times of
                        0 -> toRomanNumber' rest (nextBase base) (nextExcept base)
                        _ -> replicate times (baseToChar base) ++ toRomanNumber' rest (nextBase base) (nextExcept base)

            nextBase :: Int -> Int
            nextBase base
                | base == 10    = 5
                | otherwise     = 1

            nextExcept :: Int -> Int
            nextExcept base
                | base == 10    = 9
                | base == 5     = 4
                | otherwise     = -1

            baseToChar :: Int -> Char
            baseToChar base
                | base == 10    = 'X'
                | base == 5     = 'V'
                | base == 1     = 'I'

            ones' :: Int -> String
            ones' ones =
                case ones of
                    4 -> "IV"
                    _ -> replicate ones 'I'

            exceptToString :: Int -> String
            exceptToString except = "IX"

    tests = do
        it "returns an empty string when given 0"
            (toRomanNumber 0 == "")

        it "returns \"I\" when given 1"
            (toRomanNumber 1 == "I")

        it "returns \"V\" when given 5"
            (toRomanNumber 5 == "V")

        it "returns \"IV\" when given 4"
            (toRomanNumber 4 == "IV")

        it "returns \"VIII\" when given 8"
            (toRomanNumber 8 == "VIII")

        it "returns \"X\" when given 10"
            (toRomanNumber 10 == "X")

        it "returns \"IX\" when given 9"
            (toRomanNumber 9 == "IX")
