module FruitShopKata.Day3 (process) where

    process :: [String] -> [Int]
    process products = reverse $ process' products []
        where
            process' :: [String] -> [Int] -> [Int]
            process' [] bill = bill
            process' (p:products) []
                | p == "Pommes" = process' products (100 : [])
                | p == "Bananes" = process' products (150 : [])
                | p == "Cerises" = process' products (75 : [])
            process' (p:products) bill
                | p == "Pommes" = process' products (head bill + 100 : bill)
                | p == "Bananes" = process' products (head bill + 150 : bill)
                | p == "Cerises" = process' products (head bill + 75 : bill)
