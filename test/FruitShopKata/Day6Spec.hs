module FruitShopKata.Day6Spec (spec) where

    import Test.Hspec
    import FruitShopKata.Day6 (process)

    spec :: Spec
    spec = do
        it "should add product prices and sum" $ do
            process ["Pommes"] `shouldBe` [100]
            process ["Bananes"] `shouldBe` [150]
            process ["Pommes", "Cerises"] `shouldBe` [100, 175]

        it "should allow for reductions" $ do
            process ["Pommes", "Cerises", "Cerises"] `shouldBe` [100, 175, 230]
            process ["Cerises", "Cerises", "Cerises", "Cerises"] `shouldBe` [75, 130, 205, 260]
            process ["Cerises", "Pommes", "Cerises", "Bananes", "Cerises", "Cerises", "Pommes"] `shouldBe` [75, 175, 230, 380, 455, 510, 610]
            process ["Bananes", "Bananes"] `shouldBe` [150, 150]
            process ["Cerises", "Pommes", "Cerises", "Bananes", "Pommes", "Bananes", "Cerises"] `shouldBe` [75, 175, 230, 380, 480, 480, 555]
