module RomanNumbersKata.Day5 (toRomanNumber) where

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 50 (exceptionForBase 50)
        where
            toRomanNumber' :: Int -> Int -> Int -> String
            toRomanNumber' num base except
                | base == 1     = ones' num
                | num == except = exceptionToStr except
                | otherwise     =
                    let times   = div num base
                        rest    = mod num base
                        next    = nextBase base
                    in case times of
                        0 -> toRomanNumber' rest next (exceptionForBase next)
                        _ -> baseToChar base : toRomanNumber' rest next (exceptionForBase next)

            ones' :: Int -> String
            ones' ones
                | ones == 4 = "IV"
                | otherwise = replicate ones 'I'

            exceptionToStr :: Int -> String
            exceptionToStr except
                | except == 4   = "IV"
                | except == 9   = "IX"
                | otherwise     = ""

            exceptionForBase :: Int -> Int
            exceptionForBase base
                | base == 50    = 0
                | base == 10    = 0
                | base == 5     = 9
                | base == 1     = 4

            nextBase :: Int -> Int
            nextBase base
                | base == 50    = 10
                | base == 10    = 5
                | base == 5     = 1

            baseToChar :: Int -> Char
            baseToChar base
                | base == 50    = 'L'
                | base == 10    = 'X'
                | base == 5     = 'V'
                | base == 1     = 'I'
