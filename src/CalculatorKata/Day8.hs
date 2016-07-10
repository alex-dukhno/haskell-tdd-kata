module CalculatorKata.Day8 (tests) where

    import Test.HUnit

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

    tests = TestList
        [ TestLabel "one digit" oneDigit
        , TestLabel "many digits" manyDigits
        , TestLabel "addition" addition
        , TestLabel "subtraction" subtraction
        , TestLabel "multiplication" multiplication
        , TestLabel "division" division
        ]

    oneDigit = TestCase (assertEqual "one digit" 5.0 (calculate "5"))
    manyDigits = TestCase (assertEqual "many digits" 4568.0 (calculate "4568"))
    addition = TestCase (assertEqual "addition" (57.0+56.0) (calculate "56+57"))
    subtraction = TestCase (assertEqual "subtraction" (678.0-34.0) (calculate "678-34"))
    multiplication = TestCase (assertEqual "multiplication" (567.0*34.0) (calculate "567*34"))
    division = TestCase (assertEqual "division" (56.0/8.0) (calculate "56/8"))
