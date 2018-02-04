module UnclosedBracketsKata.Day2 (brackets) where

    brackets :: String -> Int
    brackets = foldl (\ acc v -> acc + bracketToInt v) 0
        where
            bracketToInt :: Char -> Int
            bracketToInt '(' = 1
            bracketToInt ')' = -1
