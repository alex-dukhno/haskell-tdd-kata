module LeapYearKata.Day7 (tests) where

    import Test.Hspec

    isLeapYear :: Int -> Bool
    isLeapYear year = divisibleByFour && (notDivisibleByHundred || divisibleByFourHundred)
        where
            divisibleByFour = year `mod` 4 == 0
            notDivisibleByHundred = year `mod` 100 /= 0
            divisibleByFourHundred = year `mod` 400 == 0

    tests = do
        it "is a leap year when divisible by 4" $ do
            isLeapYear 1996 `shouldBe` True

        it "is not a leap year when not divisible by 4" $ do
            isLeapYear 1997 `shouldBe` False

        it "is not a leap year when divisible by 100" $ do
            isLeapYear 1900 `shouldBe` False

        it "is a leap year when divisible by 400" $ do
            isLeapYear 2000 `shouldBe` True
