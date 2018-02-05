module BowlingGame.Kata (startGame, roll, score, Game(..)) where

    data Game = Empty | Roll Int Game
        deriving (Eq, Show)

    startGame :: Game
    startGame = Empty

    roll :: Int -> Game -> Game
    roll = Roll

    score :: Game -> Int
    score = score' 0 1
        where
            score' :: Int -> Int -> Game -> Int
            score' currentScore frameIndex game
                | frameIndex == 11  = currentScore
                | isSpare game      = score' ((framePoints game) + (spareBonus game) + currentScore) nextFrame (nextRoll game)
                | isStrike game     = score' (10 + (strikeBonus game) + currentScore) nextFrame (nextRoll game)
                | otherwise         = score' ((framePoints game) + currentScore) nextFrame (nextRoll game)
                where
                    nextFrame = frameIndex + 1

    isSpare :: Game -> Bool
    isSpare game =
        case game of    (Roll pinOne (Roll pinTwo _)) -> pinOne + pinTwo == 10
                        _ -> False

    isStrike :: Game -> Bool
    isStrike game =
        case game of    (Roll pin _) -> pin == 10
                        _ -> False

    strikeBonus :: Game -> Int
    strikeBonus game =
        case game of    (Roll _ (Roll pinOne (Roll pinTwo _))) -> pinOne + pinTwo
                        _ -> 0

    spareBonus :: Game -> Int
    spareBonus game =
        case game of    (Roll _ (Roll _ (Roll pin _))) -> pin
                        _ -> 0

    framePoints :: Game -> Int
    framePoints game =
        case game of    (Roll pinOne (Roll pinTwo _)) -> pinOne + pinTwo
                        _ -> 0

    nextRoll :: Game -> Game
    nextRoll game =
        case game of    (Roll 10 next) -> next
                        (Roll _ (Roll _ next)) -> next
                        _ -> Empty
