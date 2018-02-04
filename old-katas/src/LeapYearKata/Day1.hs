module LeapYearKata.Day1 (isLeapYear) where

    isLeapYear :: Int -> Bool
    isLeapYear year = divisibleByFour year && (divisibleByFour $ divByHundred year)
        where
            divisibleByFour :: Int -> Bool
            divisibleByFour num = num `mod` 4 == 0
            divByHundred :: Int -> Int
            divByHundred num = num `div` 100
