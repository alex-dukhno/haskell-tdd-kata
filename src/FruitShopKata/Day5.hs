module FruitShopKata.Day5 (process) where

    type Bill       = (Money, [Product])
    type Money      = Int
    type Product    = String

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
                        Just (discount, modul) -> ((applyDiscount discount) . (== 0) . (`mod` modul) . length . filter ((==) product)) (product:products)
                        Nothing -> 0

                    applyDiscount :: Money -> Bool -> Int
                    applyDiscount discount True = discount
                    applyDiscount _ _ = 0

                    specials = [("Cerises", (20, 2)), ("Bananes", (150, 2))]
