module LeapYearKata.Day3 (tests) where

    import Test.Hspec

    isLeapYear :: Int -> Bool
    isLeapYear year = isDivisibleByFour year && isDivisibleByFour (year `div` 100)
        where
            isDivisibleByFour :: Int -> Bool
            isDivisibleByFour num = num `mod` 4 == 0

    tests = do
        it "is a leap year when divisible by 4" $ do
            isLeapYear 2016 `shouldBe` True

        it "is not a leap year when not divisible by 4" $ do
            isLeapYear 2015 `shouldBe` False

        it "is not a leap year when divisible by 100" $ do
            isLeapYear 1900 `shouldBe` False

        it "is a leap year when divisible by 400" $ do
            isLeapYear 2000 `shouldBe` True
