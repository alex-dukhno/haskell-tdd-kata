module BowlingGame.Kata (score, roll, startGame, Game) where

    type Game = [Int]

    startGame :: Game
    startGame = []

    roll :: Int -> Game -> Game
    roll pin game = pin : game

    score :: Game -> Int
    score = score' 1
        where
            score' :: Int -> Game -> Int
            score' frameIndex game
                | frameIndex == 11  = 0
                | isStrike          = 10 + strikeBonus + score' nextFrameIndex nextFrame
                | isSpare           = 10 + spareBonus + score' nextFrameIndex nextFrame
                | otherwise         = framePoints + score' nextFrameIndex nextFrame
                where
                    isStrike :: Bool
                    isStrike = sum (take 1 game) == 10

                    strikeBonus :: Int
                    strikeBonus = sum (take 2 $ drop 1 game)

                    isSpare :: Bool
                    isSpare = sum (take 2 game) == 10

                    spareBonus :: Int
                    spareBonus = sum $ take 1 $ drop 2 game

                    framePoints :: Int
                    framePoints =
                        case game of    (rollOne:rollTwo:_) -> rollOne + rollTwo
                                        _                   -> 0

                    nextFrameIndex = frameIndex + 1

                    nextFrame :: Game
                    nextFrame =
                        case game of    (10:rest)   -> rest
                                        (_:_:rest)  -> rest
                                        _           -> []
