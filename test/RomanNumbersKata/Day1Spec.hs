module RomanNumbersKata.Day1Spec (spec) where

    import Test.Hspec
    import RomanNumbersKata.Day1 (toRomanNumber)

    spec :: Spec
    spec = do
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
