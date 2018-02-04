module CalculatorKata.Day2Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day2 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "3" == 3.0)

        it "calculates many digits"
            (calculate "436" == 436.0)

        it "calculates addition"
            (calculate "56+24" == 56.0+24.0)

        it "calculates subtraction"
            (calculate "78-34" == 78.0-34.0)

        it "calculates multiplication"
            (calculate "45*23" == 45.0*23.0)

        it "calculates division"
            (calculate "456/23" == 456.0/23.0)
