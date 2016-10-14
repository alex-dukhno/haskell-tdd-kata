module CalculatorKata.Day3Spec (spec) where

    import Test.Hspec
    import CalculatorKata.Day3 (calculate)

    spec :: Spec
    spec = do
        it "calculates one digit"
            (calculate "5" == 5.0)

        it "calculates many digits"
            (calculate "435" == 435.0)

        -- it "calculates addition"
        --    (calculate "45+23" == 45.0+23.0)

        -- it "calculates subtraction"
        --    (calculate "56-45" == 56.0-45.0)

        it "calculates multiplication"
            (calculate "45*2" == 45.0*2.0)

        it "calculates division"
            (calculate "56/12" == 56.0/12.0)
