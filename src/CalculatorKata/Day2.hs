module CalculatorKata.Day2 (tests) where

    import Test.HUnit

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

    tests = TestList [TestLabel "one digit" oneDigit, TestLabel "many digits" manyDigits, TestLabel "addition" addition, TestLabel "subtraction" subtraction, TestLabel "multiplication" multiplication, TestLabel "division" division, TestLabel "multiple operation" multipleOperation]

    oneDigit = TestCase (assertEqual "one digit" 3.0 (calculate "3"))
    manyDigits = TestCase (assertEqual "many digits" 436.0 (calculate "436"))
    addition = TestCase (assertEqual "addition" (56.0+24.0) (calculate "56+24"))
    subtraction = TestCase (assertEqual "subtraction" (78.0-34.0) (calculate "78-34"))
    multiplication = TestCase (assertEqual "multiplication" (45.0*23.0) (calculate "45*23"))
    division = TestCase (assertEqual "division" (456.0/23.0) (calculate "456/23"))
    multipleOperation = TestCase (assertEqual "multiple operation" (45.0-4.0+2.0*3.0+45.0/5.0) (calculate "45-4+2*3+45/5"))
