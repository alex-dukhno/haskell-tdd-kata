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
                | isStrike          = 10 + strikeBonus + score' nextIndex nextFrame
                | isSpare           = 10 + spareBonus + score' nextIndex nextFrame
                | otherwise         = framePoints + score' nextIndex nextFrame
                where
                    nextIndex = frameIndex + 1
                    isStrike :: Bool
                    isStrike =
                        case game of    (roll:_) -> roll == 10
                                        _ -> False
                    strikeBonus :: Int
                    strikeBonus =
                        case game of    (_:pinOne:pinTwo:_) -> pinOne + pinTwo
                                        _ -> 0
                    isSpare :: Bool
                    isSpare =
                        case game of    (rollOne:rollTwo:_) -> rollOne + rollTwo == 10
                                        _ -> False
                    spareBonus :: Int
                    spareBonus =
                        case game of    (_:_:pin:_) -> pin
                                        _ -> 0
                    nextFrame :: Game
                    nextFrame =
                        case game of    (10:frames) -> frames
                                        (_:_:frames) -> frames
                                        _ -> []
                    framePoints :: Int
                    framePoints =
                        case game of    (rollOne:rollTwo:_) -> rollOne + rollTwo
                                        _ -> 0
