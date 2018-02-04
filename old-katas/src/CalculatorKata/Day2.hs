module CalculatorKata.Day2 (calculate) where

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
