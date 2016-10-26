module UnclosedBracketsKata.Day6 (brackets) where

    brackets :: String -> Int
    brackets = sum . map (\c -> if c == '(' then 1 else -1)
