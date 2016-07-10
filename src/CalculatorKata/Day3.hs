module CalculatorKata.Day3 (tests) where

    import Test.HUnit

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

    tests = TestList
        [ TestLabel "one digit" oneDigit
        , TestLabel "many digits" manyDigits
        , TestLabel "addition" addition
        , TestLabel "subtraction" subtraction
        , TestLabel "multiplication" multiplication
        , TestLabel "division" division
        ]

    oneDigit = TestCase (assertEqual "one digit" 5.0 (calculate "5"))
    manyDigits = TestCase (assertEqual "many digits" 435.0 (calculate "435"))
    addition = TestCase (assertEqual "addition" (45.0+23.0) (calculate "45+23"))
    subtraction = TestCase (assertEqual "subtraction" (56.0-45.0) (calculate "56-45"))
    multiplication = TestCase (assertEqual "multiplication" (45.0*2.0) (calculate "45*2"))
    division = TestCase (assertEqual "division" (56.0/12.0) (calculate "56/12"))
