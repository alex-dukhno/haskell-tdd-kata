module LeapYearKata.Day3Spec (spec) where

    import Test.Hspec
    import LeapYearKata.Day3 (isLeapYear)

    spec :: Spec
    spec = do
        it "is a leap year when divisible by 4" $ do
            isLeapYear 2016 `shouldBe` True

        it "is not a leap year when not divisible by 4" $ do
            isLeapYear 2015 `shouldBe` False

        it "is not a leap year when divisible by 100" $ do
            isLeapYear 1900 `shouldBe` False

        it "is a leap year when divisible by 400" $ do
            isLeapYear 2000 `shouldBe` True
