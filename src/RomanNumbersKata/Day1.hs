module RomanNumbersKata.Day1 (toRomanNumber) where

    toRomanNumber :: Int -> String
    toRomanNumber num = tenth' num
        where
            tenth' :: Int -> String
            tenth' num =
                let tens = num `div` 10
                    rest = num `mod` 10
                in case tens of
                    4 -> 'X':'L':fifth' rest
                    otherwise -> (replicate tens 'X') ++ (fifth' rest)

            fifth' :: Int -> String
            fifth' num
                | num == 9 = "IX"
                | otherwise =
                    let fifth = num `div` 5
                        rest  = num `mod` 5
                    in
                        case fifth of
                            1 -> 'V': ones' rest
                            otherwise -> ones' rest

            ones' :: Int -> String
            ones' ones
                | ones == 4  = "IV"
                | otherwise = replicate ones 'I'
