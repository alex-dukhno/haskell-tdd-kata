module CalculatorKata.Day10 (tests) where

    import Test.Hspec

    calculate :: String -> Double
    calculate src = calculate' src ""
        where
            calculate' :: String -> String -> Double
            calculate' "" num = read num
            calculate' (c:src) num
                | c == '+'  = read num + calculate' src ""
                | c == '-'  = read num - calculate' src ""
                | c == '*'  = read num * calculate' src ""
                | c == '/'  = read num / calculate' src ""
                | otherwise = calculate' src (num ++ [c])

    tests = do
        it "calculates one digit"
            (calculate "9" == 9.0)

        it "calculates many digits"
            (calculate "457" == 457.0)

        it "calculates addition"
            (calculate "456+234" == 456.0+234.0)

        it "calculates subtraction"
            (calculate "4365-23" == 4365.0-23.0)

        it "calculates multiplication"
            (calculate "435*12" == 435.0*12.0)

        it "calculates division"
            (calculate "4353456/23" == 4353456.0/23.0)
