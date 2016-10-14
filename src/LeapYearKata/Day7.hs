module LeapYearKata.Day7 (isLeapYear) where

    isLeapYear :: Int -> Bool
    isLeapYear year = divisibleByFour && (notDivisibleByHundred || divisibleByFourHundred)
        where
            divisibleByFour = year `mod` 4 == 0
            notDivisibleByHundred = year `mod` 100 /= 0
            divisibleByFourHundred = year `mod` 400 == 0
