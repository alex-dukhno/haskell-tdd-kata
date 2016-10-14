module CalculatorKata.Day3 (calculate) where

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
