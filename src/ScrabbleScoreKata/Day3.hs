module ScrabbleScoreKata.Day3 (score) where

    import Data.Char (toLower)

    letters = [('a', 1), ('t', 1), ('o', 1), ('s', 1), ('r', 1), ('e', 1), ('u', 1), ('i', 1), ('n', 1)
                , ('p', 3), ('b', 3)
                , ('f', 4), ('y', 4), ('h', 4)
                , ('k', 5)
                , ('x', 8)
                , ('z', 10), ('q', 10)]

    score :: String -> Int
    score []        = 0
    score (c:src)   = letterValue (toLower c) + score(src)
        where
            letterValue :: Char -> Int
            letterValue c = case lookup c letters of
                Just v  -> v
                Nothing -> 0
