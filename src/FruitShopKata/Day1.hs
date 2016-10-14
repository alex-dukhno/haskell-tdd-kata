module FruitShopKata.Day1 (process) where

    type Bill       = (Money, [Product])
    type Money      = Int
    type Product    = String

    process :: [Product] -> [Money]
    process = map fst . tail . scanl addProduct (0, [])
        where
            addProduct :: Bill -> Product -> Bill
            addProduct (total, products) product = (total + findPrice product - reductions product, product:products)
                where
                    findPrice :: Product -> Money
                    findPrice product = case lookup product fruits of
                        Just price -> price
                        Nothing -> error $ product ++ " not found"

                    reductions :: Product -> Money
                    reductions product = case lookup product specials of
                        Just (discount, modul) -> ((applyReduction discount) . (==0) . (`mod` modul) . length . filter ((==) product)) (product:products)
                        Nothing -> 0

                    applyReduction :: Int -> Bool -> Int
                    applyReduction discount True    = discount
                    applyReduction _ _              = 0

                    fruits = [("Pommes",100),("Bananes",150),("Cerises",75)]

                    specials = [("Cerises", (20,2)), ("Bananes", (150,2))]
