module ScrabbleScoreKata.Day1 (score) where

    import Data.Char (toLower)

    score :: String -> Int
    score input = foldl (+) 0 src
        where
            src = map letterValue input
            letterValue :: Char -> Int
            letterValue l = case lookup letter letters of
                Just v  -> v
                Nothing -> 0
                where letter = toLower l

            letters = [('a', 1), ('t', 1) , ('o', 1), ('s', 1), ('r', 1), ('e', 1),('u', 1), ('i', 1), ('n', 1)
                        , ('p', 3), ('b', 3)
                        , ('f', 4), ('y', 4), ('h', 4)
                        , ('k', 5)
                        , ('x', 8)
                        , ('z', 10), ('q', 10)]
