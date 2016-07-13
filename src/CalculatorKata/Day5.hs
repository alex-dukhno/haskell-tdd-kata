module CalculatorKata.Day5 (tests) where

    import Data.Char
    import Test.Hspec

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

    tests = do
        it "calculates one digit"
            (calculate "7" == 7.0)

        it "calculates many digits"
            (calculate "547" == 547.0)

        it "calculates addition"
            (calculate "54+78" == 54.0+78.0)

        it "calculates subtraction"
            (calculate "67-34" == 67.0-34.0)

        it "calculates multiplication"
            (calculate "45*4" == 45.0*4.0)

        it "calculates division"
            (calculate "657/34" == 657.0/34.0)
