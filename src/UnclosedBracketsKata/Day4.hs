module UnclosedBracketsKata.Day4 (brackets) where

    brackets :: String -> Int
    brackets = sum . map bracketToInt
        where
            bracketToInt :: Char -> Int
            bracketToInt '(' = 1
            bracketToInt ')' = -1
