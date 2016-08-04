module FruitShopKata.Day4 (tests) where

    import Test.Hspec

    type Bill       = (Money, [Product])
    type Money      = Int
    type Product    = String

    process :: [Product] -> [Money]
    -- process = map fst . tail . scanl addProduct (0, [])
    process products = map fst (tail (scanl addProduct (0, []) products))
        where
            addProduct :: Bill -> Product -> Bill
            addProduct (total, products) product = (total + findPrice product - discounts product, product:products)
                where
                    findPrice :: Product -> Money
                    findPrice product
                        | product == "Pommes"   = 100
                        | product == "Cerises"  = 75
                        | product == "Bananes"  = 150

                    discounts :: Product -> Money
                    discounts product =
                        case lookup product specials of
                            Just (discount, m) -> ((applyDiscount discount) . (== 0) . (`mod` m) . length . filter ((==) product)) (product:products)
                            Nothing -> 0

                    applyDiscount :: Money -> Bool -> Money
                    applyDiscount discount True = discount
                    applyDiscount _ _           = 0

                    specials = [("Cerises", (20, 2)), ("Bananes", (150, 2))]

    tests = do
        it "should add product prices and sum up" $ do
            process ["Pommes"] `shouldBe` [100]
            process ["Bananes"] `shouldBe` [150]
            process ["Pommes", "Cerises"] `shouldBe` [100, 175]

        it "should allow discounts" $ do
            process ["Pommes", "Cerises", "Cerises"] `shouldBe` [100, 175, 230]
            process ["Cerises", "Cerises", "Cerises", "Cerises"] `shouldBe` [75, 130, 205, 260]
            process ["Cerises", "Pommes", "Cerises", "Bananes", "Cerises", "Cerises", "Pommes"] `shouldBe` [75, 175, 230, 380, 455, 510, 610]
            process ["Bananes", "Bananes"] `shouldBe` [150, 150]
            process ["Cerises", "Pommes", "Cerises", "Bananes", "Pommes", "Bananes", "Cerises"] `shouldBe` [75, 175, 230, 380, 480, 480, 555]
