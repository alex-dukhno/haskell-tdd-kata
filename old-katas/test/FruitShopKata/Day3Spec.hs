module FruitShopKata.Day3Spec (spec) where

    import Test.Hspec
    import FruitShopKata.Day3 (process)

    spec :: Spec
    spec = do
        it "should add product" $ do
            process ["Pommes"] `shouldBe` [100]

        it "should sum up cost of products" $ do
            process ["Pommes", "Bananes", "Cerises"] `shouldBe` [100, 250, 325]
