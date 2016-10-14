module CalculatorKata.Day7Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day7 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "6" == 6.0)

        it "calculates many digits"
            (calculate "457" == 457.0)

        it "calculates addition"
            (calculate "45+56" == 45.0+56.0)

        it "calculates subtraction"
            (calculate "675-34" == 675.0-34.0)

        it "calculates multiplication"
            (calculate "56*2" == 56.0*2.0)

        it "calculates division"
            (calculate "56/45" == 56.0/45.0)
