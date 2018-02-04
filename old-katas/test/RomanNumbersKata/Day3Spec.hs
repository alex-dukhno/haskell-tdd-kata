module RomanNumbersKata.Day3Spec (spec) where

    import Test.Hspec
    import RomanNumbersKata.Day3 (toRomanNumber)

    spec :: Spec
    spec = do
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
