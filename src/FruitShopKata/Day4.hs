module FruitShopKata.Day4 (process) where

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
