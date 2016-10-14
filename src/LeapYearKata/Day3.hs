module LeapYearKata.Day3 (isLeapYear) where

    isLeapYear :: Int -> Bool
    isLeapYear year = isDivisibleByFour year && isDivisibleByFour (year `div` 100)
        where
            isDivisibleByFour :: Int -> Bool
            isDivisibleByFour num = num `mod` 4 == 0
