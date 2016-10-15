module SubListKata.Day1 (sublist, Result(..)) where

    data Result = Equal | SubList | SuperList | Unequal
        deriving (Eq, Show)

    sublist :: (Ord a) => [a] -> [a] -> Result
    sublist [] []   = Equal
    sublist [] _    = SubList
    sublist _ []    = SuperList
    sublist l1 l2
        | l1 == l2 = Equal
        | otherwise = Unequal
