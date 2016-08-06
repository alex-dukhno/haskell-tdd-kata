module FruitShopKata.Day6 (tests) where

    import Test.Hspec

    type Bill       = (Money, [Product])
    type Product    = String
    type Money      = Int

    process :: [Product] -> [Money]
    process = map fst . tail . scanl addProduct (0, [])
        where
            addProduct :: Bill -> Product -> Bill
            addProduct (total, products) product = (total + findPrice product - discount product, product:products)
                where
                    findPrice :: Product -> Money
                    findPrice product
                        | product == "Pommes" = 100
                        | product == "Cerises" = 75
                        | product == "Bananes" = 150

                    discount :: Product -> Money
                    discount product = case lookup product specials of
                        Just (discount, modul) -> ((applyDiscount discount) . (==0) . (`mod` modul) . length . filter ((==) product)) (product:products)
                        Nothing -> 0

                    applyDiscount :: Money -> Bool -> Money
                    applyDiscount discount True = discount
                    applyDiscount _ _           = 0

                    specials = [("Cerises", (20, 2)), ("Bananes", (150, 2))]

    tests = do
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
