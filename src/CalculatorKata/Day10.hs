module CalculatorKata.Day10 (tests) where

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
        ]

    oneDigit        = TestCase (assertEqual "one digit" 9.0 (calculate "9"))
    manyDigits      = TestCase (assertEqual "many digits" 457.0 (calculate "457"))
    addition        = TestCase (assertEqual "addition" (456.0+234.0) (calculate "456+234"))
    subtraction     = TestCase (assertEqual "subtraction" (4365.0-23.0) (calculate "4365-23"))
    multiplication  = TestCase (assertEqual "multiplication" (435.0*12.0) (calculate "435*12"))
    division        = TestCase (assertEqual "division" (4353456.0/23.0) (calculate "4353456/23"))
