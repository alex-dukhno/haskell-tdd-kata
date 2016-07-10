module CalculatorKata.Day5 (tests) where

    import Data.Char
    import Test.HUnit

    calculate :: String -> Double
    calculate src = fst (parseExpression src)
        where
            parseExpression :: String -> (Double, String)
            parseExpression ""  = (0.0, "")
            parseExpression str =
                let (term, rest) = parseTerm str
                in case rest of
                    ""      -> (term, "")
                    c:rest' ->
                        case c of
                            '+' -> let (term', rest'')   = parseTerm rest' in (term + term', rest'')
                            '-' -> let (term', rest'')   = parseTerm rest' in (term - term', rest'')
                            _   -> (term, rest)

            parseTerm :: String -> (Double, String)
            parseTerm ""    = (0.0, "")
            parseTerm str   =
                let (arg, rest) = parseArg str
                in case rest of
                    ""      -> (arg, "")
                    c:rest' ->
                        case c of
                            '*' -> let (arg', rest'') = parseArg rest' in (arg * arg', rest'')
                            '/' -> let (arg', rest'') = parseArg rest' in (arg / arg', rest'')
                            _   -> (arg, rest)

            parseArg :: String -> (Double, String)
            parseArg ""     = (0.0, "")
            parseArg str    = let (digits, rest) = span isDigit str in (read digits, rest)

    tests = TestList
            [ TestLabel "one digit" oneDigit
            , TestLabel "many digits" manyDigits
            , TestLabel "addition" addition
            , TestLabel "subtraction" subtraction
            , TestLabel "multiplication" multiplication
            , TestLabel "division" division
            , TestLabel "many operations" manyOperations]

    oneDigit = TestCase (assertEqual "one digit" 7.0 (calculate "7"))
    manyDigits = TestCase (assertEqual "many digits" 547.0 (calculate "547"))
    addition = TestCase (assertEqual "addition" (54.0+78.0) (calculate "54+78"))
    subtraction = TestCase (assertEqual "subtraction" (67.0-34.0) (calculate "67-34"))
    multiplication = TestCase (assertEqual "multiplication" (45.0*4.0) (calculate "45*4"))
    division = TestCase (assertEqual "division" (657.0/34.0) (calculate "657/34"))
    manyOperations = TestCase (assertEqual "multiple operation" (45.0-4.0+2.0*3.0+45.0/5.0) (calculate "45-4+2*3+45/5"))
