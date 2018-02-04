module CalculatorKata.Day1 (calculate) where

    import Data.List (splitAt)

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
