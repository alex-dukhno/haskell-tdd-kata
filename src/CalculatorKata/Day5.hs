module CalculatorKata.Day5 (calculate) where

    import Data.Char

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
