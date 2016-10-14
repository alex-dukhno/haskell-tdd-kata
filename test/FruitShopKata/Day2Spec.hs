module FruitShopKata.Day2Spec (spec) where

    import Test.Hspec
    import FruitShopKata.Day2 (process)

    spec :: Spec
    spec = do
        it "should add product" $ do
            process ["Pommes"] `shouldBe` [100]

        it "should sum up products prices" $ do
            process ["Pommes", "Bananes", "Cerises"] `shouldBe` [100, 250, 325]

        -- it "should take into account discounts" $ do
            -- process ["Bananes", "Bananes"] `shouldBe` [150, 150]
