module CalculatorKata.Day6 (tests) where

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

    tests = TestList [TestLabel "one digit" oneDigit, TestLabel "many digits" manyDigits, TestLabel "addition" addition, TestLabel "subtraction" subtraction, TestLabel "multiplication" multiplication, TestLabel "division" division]

    oneDigit = TestCase (assertEqual "one digit" 5.0 (calculate "5"))
    manyDigits = TestCase (assertEqual "many digits" 5467.0 (calculate "5467"))
    addition = TestCase (assertEqual "addition" (54.0+26.0) (calculate "54+26"))
    subtraction = TestCase (assertEqual "subtraction" (76.0-23.0) (calculate "76-23"))
    multiplication = TestCase (assertEqual "multiplication" (45.0*23.0) (calculate "45*23"))
    division = TestCase (assertEqual "division" (567.0/34.0) (calculate "567/34"))
