module LeapYearKata.Day1 (tests) where

    import Test.Hspec

    isLeapYear :: Int -> Bool
    isLeapYear year = divisibleByFour year && (divisibleByFour $ divByHundred year)
        where
            divisibleByFour :: Int -> Bool
            divisibleByFour num = num `mod` 4 == 0
            divByHundred :: Int -> Int
            divByHundred num = num `div` 100

    tests = do
        it "is a leap year when divisible by 4" $ do
            isLeapYear 2016

        it "is not a leap year when not divisible by 4" $ do
            not $ isLeapYear 2015

        it "is not a leap year when divisible by 100" $ do
            not $ isLeapYear 1900

        it "is a leap year when divisible by 4 and 100" $ do
            isLeapYear 2000
