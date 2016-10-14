module CalculatorKata.Day6Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day6 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "5" == 5.0)

        it "calculates many digits"
            (calculate "5467" == 5467.0)

        it "calculates addition"
            (calculate "54+26" == 54.0+26.0)

        it "calculates subtraction"
            (calculate "76-23" == 76.0-23.0)

        it "calculates multiplication"
            (calculate "45*23" == 45.0*23.0)

        it "calculates division"
            (calculate "567/34" == 567.0/34.0)
