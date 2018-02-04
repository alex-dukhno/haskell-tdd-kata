module RomanNumbersKata.Day6Spec (spec) where

    import Test.Hspec
    import RomanNumbersKata.Day6 (toRomanNumber)

    spec :: Spec
    spec = do
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
