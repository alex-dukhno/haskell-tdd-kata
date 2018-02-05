module BowlingGame.Kata (score) where

    import Data.List (genericIndex)

    score :: [Int] -> Int
    score = score' 0 1

        where
            score' :: Int -> Int -> [Int] -> Int
            score' currentScore frameIndex rolls
                | frameIndex == 11  = currentScore
                | isStrike          = score' (computeScore' 10 strikeBonus) nextFrameIndex (drop 1 rolls)
                | isSpare           = score' (computeScore' 10 spareBonus) nextFrameIndex (drop 2 rolls)
                | otherwise         = score' (computeScore' framePoints 0) nextFrameIndex (drop 2 rolls)

                where
                    nextFrameIndex = frameIndex + 1

                    computeScore' :: Int -> Int -> Int
                    computeScore' framePins bonus = framePins + bonus + currentScore

                    isStrike :: Bool
                    isStrike = rolls !! 0 == 10

                    strikeBonus :: Int
                    strikeBonus = sum(take 2 (drop 1 rolls))

                    isSpare :: Bool
                    isSpare = framePoints == 10

                    spareBonus :: Int
                    spareBonus = rolls !! 2

                    framePoints :: Int
                    framePoints = sum (take 2 rolls)
