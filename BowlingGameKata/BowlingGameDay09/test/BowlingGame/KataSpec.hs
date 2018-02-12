module BowlingGame.KataSpec (spec) where

  import Test.Hspec
  import BowlingGame.Kata (score, roll, startGame, Game, Pin)

  spec :: Spec
  spec =
    describe "Bowling Game" $ do
      it "processes gutter game" $
        score (rollMany 20 0 startGame) `shouldBe` 0

      it "processes all ones" $
        score (rollMany 20 1 startGame) `shouldBe` 20

      it "processes one spare" $
        score (rollSpare $ roll 3 $ rollMany 17 0 startGame) `shouldBe` 16

      it "processes one strike" $
        score (rollStrike $ roll 4 $ roll 3 $ rollMany 16 0 startGame) `shouldBe` 24

      it "processes perfect game" $
        score (rollMany 12 10 startGame) `shouldBe` 300

  rollMany :: Int -> Pin -> Game -> Game
  rollMany times pin game
      | times == 0 = game
      | otherwise   = rollMany (times - 1) pin (roll pin game)

  rollSpare :: Game -> Game
  rollSpare = roll 4 . roll 6

  rollStrike :: Game -> Game
  rollStrike = roll 10
