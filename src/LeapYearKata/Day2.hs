module LeapYearKata.Day2 (isLeapYear) where

    isLeapYear :: Int -> Bool
    isLeapYear year = (isDivisibleByFour $ div year 100) && isDivisibleByFour year
        where
            isDivisibleByFour :: Int -> Bool
            isDivisibleByFour num = num `mod` 4 == 0
