module CalculatorKata.Day9 (tests) where

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
            (calculate "5467" == 5467.0)

        it "calculates addition"
            (calculate "5436.0+3245.0" == 5436.0+3245.0)

        it "calculates subtraction"
            (calculate "546-3254" == 546.0-3254.0)

        it "calculates multiplication"
            (calculate "4536*123" == 4536.0*123.0)

        it "calculates division"
            (calculate "4356/213" == 4356.0/213)
