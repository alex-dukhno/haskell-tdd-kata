module CalculatorKata.Day1 ( tests ) where

    import Test.HUnit
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

    tests = TestList [TestLabel "one digit" oneDigit, TestLabel "many digit" manyDigit, TestLabel "addition" addition, TestLabel "subtraction" subtraction, TestLabel "multiplication" multiplication, TestLabel "division" division]

    oneDigit = TestCase (assertEqual "one digit" 2.0 (calculate "2"))
    manyDigit = TestCase (assertEqual "many digit" 234.0 (calculate "234"))
    addition = TestCase (assertEqual "addition" (54.0+6.0) (calculate "54+6"))
    subtraction = TestCase (assertEqual "subtraction" (54.0-8.0) (calculate "54-8"))
    multiplication = TestCase (assertEqual "multiplication" (54.0*2) (calculate "54*2"))
    division = TestCase (assertEqual "division" (54.0/6.0) (calculate "54/6"))
