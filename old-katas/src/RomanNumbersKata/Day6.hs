module RomanNumbersKata.Day6 (toRomanNumber) where

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 50 (exceptionForBase 50)
        where
            toRomanNumber' :: Int -> Int -> Int -> String
            toRomanNumber' num base exception
                | base == 1         = ones' num
                | exception == num  = exceptionToStr exception
                | otherwise         =
                    let times   = div num base
                        rest    = mod num base
                        next    = nextBase base
                    in case times of
                        0 -> toRomanNumber' rest next (exceptionForBase next)
                        _ -> replicate times (baseToChar base) ++ toRomanNumber' rest next (exceptionForBase next)

            exceptionForBase :: Int -> Int
            exceptionForBase base
                | base == 10    = 40
                | base == 5     = 9
                | base == 1     = 4
                | otherwise     = 0

            ones' :: Int -> String
            ones' ones
                | ones == 4 = "IV"
                | otherwise = replicate ones 'I'

            exceptionToStr :: Int -> String
            exceptionToStr exception
                | exception == 40   = "XL"
                | exception == 9    = "IX"
                | exception == 4    = "IV"
                | otherwise         = ""

            nextBase :: Int -> Int
            nextBase base
                | base == 50    = 10
                | base == 10    = 5
                | otherwise     = 1

            baseToChar :: Int -> Char
            baseToChar num
                | num == 50 = 'L'
                | num == 10 = 'X'
                | num == 5  = 'V'
                | num == 1  = 'I'
