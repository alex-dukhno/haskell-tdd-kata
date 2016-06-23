module Assert where

assert name actual expected = do
    if actual == expected
        then print $ name ++ " PASSED"
        else print $ name ++ " FAILED expected <" ++ show expected ++ "> but was <" ++ show actual ++ ">"
