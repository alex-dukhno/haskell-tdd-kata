module LeapYearKata.Day1Spec (spec) where

    import Test.Hspec
    import LeapYearKata.Day1 (isLeapYear)

    spec :: Spec
    spec = do
        it "is a leap year when divisible by 4" $ do
            isLeapYear 2016

        it "is not a leap year when not divisible by 4" $ do
            not $ isLeapYear 2015

        it "is not a leap year when divisible by 100" $ do
            not $ isLeapYear 1900

        it "is a leap year when divisible by 4 and 100" $ do
            isLeapYear 2000
