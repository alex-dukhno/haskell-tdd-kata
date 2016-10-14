module CalculatorKata.Day5Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day5 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "7" == 7.0)

        it "calculates many digits"
            (calculate "547" == 547.0)

        it "calculates addition"
            (calculate "54+78" == 54.0+78.0)

        it "calculates subtraction"
            (calculate "67-34" == 67.0-34.0)

        it "calculates multiplication"
            (calculate "45*4" == 45.0*4.0)

        it "calculates division"
            (calculate "657/34" == 657.0/34.0)
