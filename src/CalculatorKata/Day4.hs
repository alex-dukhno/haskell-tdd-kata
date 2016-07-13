module CalculatorKata.Day4 (tests) where

    import Test.Hspec

    calculate :: String -> Double
    calculate src = calculate' src []
        where
            calculate' [] num = read num
            calculate' (c:src) num  | c == '+' = read num + calculate' src []
                                    | c == '-' = read num - calculate' src []
                                    | c == '*' = read num * calculate' src []
                                    | c == '/' = read num / calculate' src []
                                    | otherwise = calculate' src (num ++ [c])

    tests = do
        it "calculates one digit"
            (calculate "4" == 4.0)

        it "calculates many digits"
            (calculate "345" == 345.0)

        it "calculates addition"
            (calculate "56+78" == 56.0+78.0)

        it "calculates subtraction"
            (calculate "768-45" == 768.0-45.0)

        it "calculates multiplication"
            (calculate "65*2" == 65.0*2.0)

        it "calculates division"
            (calculate "66/3" == 66.0/3.0)
