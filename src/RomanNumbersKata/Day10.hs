module RomanNumbersKata.Day10 (toRomanNumber) where

    toRomanNumber :: Int -> String
    toRomanNumber num = toRomanNumber' num 50
        where
            toRomanNumber' :: Int -> Int -> String
            toRomanNumber' num base =
                case base of
                    1 ->
                        case num of
                            4 -> "IV"
                            _ -> replicate num 'I'
                    _ ->
                        let times   = div num base
                            rest    = mod num base
                            next    = nextBase base
                        in case times of
                            0 -> toRomanNumber' rest next
                            _ -> baseToChar base : toRomanNumber' rest next

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
