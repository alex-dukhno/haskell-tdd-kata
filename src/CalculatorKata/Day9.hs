module CalculatorKata.Day9 (tests) where

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
        [ TestLabel "one digit"         oneDigit
        , TestLabel "many digits"       manyDigits
        , TestLabel "addition"          addition
        , TestLabel "subtraction"       subtraction
        , TestLabel "multiplication"    multiplication
        , TestLabel "division"          division
        ]

    oneDigit        = TestCase (assertEqual "one digit" 6.0 (calculate "6"))
    manyDigits      = TestCase (assertEqual "many digits" 5467.0 (calculate "5467"))
    addition        = TestCase (assertEqual "addition" (5436.0+3245.0) (calculate "5436.0+3245.0"))
    subtraction     = TestCase (assertEqual "subtraction" (546.0-3254.0) (calculate "546-3254"))
    multiplication  = TestCase (assertEqual "multiplication" (4536.0*123.0) (calculate "4536*123"))
    division        = TestCase (assertEqual "division" (4356.0/213) (calculate "4356/213"))
