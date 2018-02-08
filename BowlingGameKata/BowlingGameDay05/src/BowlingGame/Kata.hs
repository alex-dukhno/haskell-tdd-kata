module BowlingGame.Kata (score, roll, Game, startGame) where

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
                    isStrike =
                        case game of    (roll:_)    -> roll == 10
                                        _           -> False

                    strikeBonus :: Int
                    strikeBonus =
                        case game of    (_:rollOne:rollTwo:_)   -> rollOne + rollTwo
                                        _                       -> 0

                    isSpare :: Bool
                    isSpare =
                        case game of    (rollOne:rollTwo:_) -> rollOne + rollTwo == 10
                                        _                   -> False

                    spareBonus :: Int
                    spareBonus =
                        case game of    (_:_:roll:_)    -> roll
                                        _               -> 0

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
