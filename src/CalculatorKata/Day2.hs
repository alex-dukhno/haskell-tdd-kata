module CalculatorKata.Day2 (tests) where

    import Test.Hspec

    calculate :: String -> Double
    calculate source = do
        let firstArgList = takeWhile (\c -> c /= '+' && c /= '-' && c /= '*' && c /= '/') source
        if length firstArgList /= length source
            then do
                let sign = head (snd (splitAt (length firstArgList) source))
                let secondArgList = tail (snd (splitAt (length firstArgList) source))
                let first = read firstArgList
                let second = read secondArgList
                case sign of    '+' -> first + second
                                '-' -> first - second
                                '*' -> first * second
                                '/' -> first / second
            else
                read firstArgList

    parseExpression :: String -> Double
    parseExpression expr = 0

    parseTerm :: String -> Double
    parseTerm term = 0

    parseArg :: String -> String -> Double
    parseArg [] arg = read arg
    parseArg (c:chars) arg  | c `elem` "+-*/" = read arg
                            | otherwise = parseArg chars (arg ++ [c])

    tests = do
        it "calculates one digit"
            (calculate "3" == 3.0)

        it "calculates many digits"
            (calculate "436" == 436.0)

        it "calculates addition"
            (calculate "56+24" == 56.0+24.0)

        it "calculates subtraction"
            (calculate "78-34" == 78.0-34.0)

        it "calculates multiplication"
            (calculate "45*23" == 45.0*23.0)

        it "calculates division"
            (calculate "456/23" == 456.0/23.0)
