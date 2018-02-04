module UnclosedBracketsKata.Day1 (brackets) where

    brackets :: String -> Int
    brackets input = foldl bracketToInt 0 input
        where
            bracketToInt :: Int -> Char -> Int
            bracketToInt acc '('    = acc + 1
            bracketToInt acc ')'    = acc -1
