module CalculatorKata.Day9Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day9 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "6" == 6.0)

        it "calculates many digits"
            (calculate "5467" == 5467.0)

        it "calculates addition"
            (calculate "5436.0+3245.0" == 5436.0+3245.0)

        it "calculates subtraction"
            (calculate "546-3254" == 546.0-3254.0)

        it "calculates multiplication"
            (calculate "4536*123" == 4536.0*123.0)

        it "calculates division"
            (calculate "4356/213" == 4356.0/213)
