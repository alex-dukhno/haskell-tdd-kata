module CalculatorKata.Day1Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day1 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "2" == 2.0)

        it "calculates many digits"
            (calculate "234" == 234.0)

        it "calculates addition"
            (calculate "54+6" == 54.0+6.0)

        it "calculates subtraction"
            (calculate "54-8" == 54.0-8.0)

        it "calculates multiplication"
            (calculate "54*2" == 54.0*2)

        it "calculates division"
            (calculate "54/6" == 54.0/6.0)
