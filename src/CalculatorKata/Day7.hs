module CalculatorKata.Day7 (tests) where

    import Test.Hspec

    calculate :: String -> Double
    calculate src = calculate' src ""
        where
            calculate' :: String -> String -> Double
            calculate' "" num = read num
            calculate' (c:src) num  | c == '+'  = read num + calculate' src ""
                                    | c == '-'  = read num - calculate' src ""
                                    | c == '*'  = read num * calculate' src ""
                                    | c == '/'  = read num / calculate' src ""
                                    | otherwise = calculate' src (num ++ [c])

    tests = do
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
