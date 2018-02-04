module SubListKata.Day3 (sublist, Result(..)) where

    import Data.List (isInfixOf)

    data Result = Equal | Unequal | Sublist | Superlist
        deriving (Eq, Show)

    sublist :: (Ord a) => [a] -> [a] -> Result
    sublist [] []   = Equal
    sublist [] _    = Sublist
    sublist _ []    = Superlist
    sublist l1 l2
        | l1 == l2  = Equal
        | isInfixOf l1 l2 = Sublist
        | isInfixOf l2 l1 = Superlist
        | otherwise = Unequal
