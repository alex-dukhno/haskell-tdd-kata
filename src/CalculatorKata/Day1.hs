module CalculatorKata.Day1 ( tests ) where

    import Test.Hspec
    import Data.List (splitAt)
    import Debug.Trace (trace)

    calculate :: String -> Double
    calculate source = do
        let argOneList = takeWhile (\c -> c /= '+' && c /= '-' && c /= '*' && c /= '/') source
        let argOne = read argOneList
        let lengthOne = length argOneList
        if lengthOne /= length source
            then do
                let sign = head (snd (splitAt lengthOne source))
                let argTwoList = tail (snd (splitAt lengthOne source))
                case sign of    '+' -> argOne + (read argTwoList)
                                '-' -> argOne - (read argTwoList)
                                '*' -> argOne * (read argTwoList)
                                '/' -> argOne / (read argTwoList)
            else
                argOne

    tests = do
        it "calculates one digit"
            (calculate "2" == 2.0)

        it "calculates many digits"
            (calculate "234" == 234.0)

        it "calculates addition"
            (calculate "54+6" == 54.0+6.0)

        it "calculates subtraction"
            (calculate "54-8" == 54.0-8.0)

        it "calculates multiplication"
            (calculate "54*2" == 54.0*2)

        it "calculates division"
            (calculate "54/6" == 54.0/6.0)
