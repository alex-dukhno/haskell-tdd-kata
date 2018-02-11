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
          isStrike = game !! 0 == 10
          strikeBonus = sum (take 2 $ drop 1 game)
          isSpare = sum (take 2 game) == 10
          spareBonus = game !! 2
          framePoints = sum $ take 2 game
          nextFrameIndex = frameIndex + 1
          nextFrame = 
            case game of  (10:rest) -> rest
                          _         -> drop 2 game