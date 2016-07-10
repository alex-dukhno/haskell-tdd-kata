module CalculatorKata.Day4 (tests) where

    import Test.HUnit

    calculate :: String -> Double
    calculate src = calculate' src []
        where
            calculate' [] num = read num
            calculate' (c:src) num  | c == '+' = read num + calculate' src []
                                    | c == '-' = read num - calculate' src []
                                    | c == '*' = read num * calculate' src []
                                    | c == '/' = read num / calculate' src []
                                    | otherwise = calculate' src (num ++ [c])

    tests = TestList
        [ TestLabel "one digit" oneDigit
        , TestLabel "many digits" manyDigits
        , TestLabel "addition" addition
        , TestLabel "subtraction" subtraction
        , TestLabel "multiplication" multiplication
        , TestLabel "division" division
        ]

    oneDigit = TestCase (assertEqual "one digit" 4.0 (calculate "4"))
    manyDigits = TestCase (assertEqual "many digits" 345.0 (calculate "345"))
    addition = TestCase (assertEqual "addition" (56.0+78.0) (calculate "56+78"))
    subtraction = TestCase (assertEqual "subtraction" (768.0-45.0) (calculate "768-45"))
    multiplication = TestCase (assertEqual "multiplication" (65.0*2.0) (calculate "65*2"))
    division = TestCase (assertEqual "division" (66.0/3.0) (calculate "66/3"))
