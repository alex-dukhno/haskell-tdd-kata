module CalculatorKata.Day7 (tests) where

    import Test.HUnit

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

    tests = TestList
        [ TestLabel "one digit" oneDigit
        , TestLabel "many digits" manyDigits
        , TestLabel "addition" addition
        , TestLabel "subtraction" subtraction
        , TestLabel "multiplication" multiplication
        , TestLabel "division" division
        ]

    oneDigit = TestCase (assertEqual "one digit" 6.0 (calculate "6"))
    manyDigits = TestCase (assertEqual "many digits" 457.0 (calculate "457"))
    addition = TestCase (assertEqual "addition" (45.0+56.0) (calculate "45+56"))
    subtraction = TestCase (assertEqual "subtraction" (675.0-34.0) (calculate "675-34"))
    multiplication = TestCase (assertEqual "multiplication" (56.0*2.0) (calculate "56*2"))
    division = TestCase (assertEqual "division" (56.0/45.0) (calculate "56/45"))
