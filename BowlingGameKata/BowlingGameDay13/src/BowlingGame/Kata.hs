module BowlingGame.Kata (score, roll, startGame, Game, Pin) where

  type Game = [Int]
  type Pin = Int

  startGame :: Game
  startGame = []

  roll :: Pin -> Game -> Game
  roll pin game = pin : game

  score :: Game -> Int
  score = score' 1
    where
      score' :: Int -> Game -> Int
      score' frameIndex game
        | frameIndex == 11  = 0
        | isStrike          = 10 + strikeBonus + score' nextFrameIndex strikeFrame
        | isSpare           = 10 + spareBonus + score' nextFrameIndex nextFrame
        | otherwise         = framePoints + score' nextFrameIndex nextFrame
          where
            isStrike = head game == 10
            strikeBonus = sum $ take 2 $ drop 1 game
            strikeFrame = tail game
            isSpare = framePoints == 10
            spareBonus = game !! 2
            framePoints = sum $ take 2 game
            nextFrameIndex = frameIndex + 1
            nextFrame = drop 2 game
