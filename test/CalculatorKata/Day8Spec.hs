module CalculatorKata.Day8Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day8 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "5" == 5.0)

        it "calculates many digits"
            (calculate "4568" == 4568.0)

        it "calculates addition"
            (calculate "56+57" == 57.0+56.0)

        it "calculates subtraction"
            (calculate "678-34" == 678.0-34.0)

        it "calculates multiplication"
            (calculate "567*34" == 567.0*34.0)

        it "calculates division"
            (calculate "56/8" == 56.0/8.0)
