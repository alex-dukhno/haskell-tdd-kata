module CalculatorKata.Day3 (tests) where

    import Test.Hspec

    calculate :: String -> Double
    calculate src = calculate' src []
        where
            calculate' [] [] = 0.0
            calculate' [] num = read num
            calculate' (c:src) num  | c == '+' = read num + parseTerm src []
                                    | c == '-' = read num - parseTerm src []
                                    | otherwise = parseTerm (c:src) num
            parseTerm [] [] = 0.0
            parseTerm [] num = read num
            parseTerm (c:src) num   | c == '*' = read num * parseTerm src []
                                    | c == '/' = read num / parseTerm src []
                                    | elem c "+-" = read num
                                    | otherwise = parseTerm src (num ++ [c])

    tests = do
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
