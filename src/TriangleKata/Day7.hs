module TriangleKata.Day7 (tests) where

    import Test.Hspec

    data TriangleType = Illegal | Equilateral | Isosceles | Scalene
        deriving (Eq, Show)

    type Triangle = (Int, Int, Int)

    triangle :: Triangle -> TriangleType
    triangle (a, b, c)
        | a + b <= c
            || b + c <= a
            || c + a <= b   = Illegal
        | a == b
            && b == c
            && c == a   = Equilateral
        | a == b
            || b == c
            || c == a   = Isosceles
        | otherwise     = Scalene

    tests = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle has first two sides equal" $ do
            triangle (6, 6, 8) `shouldBe` Isosceles

        it "isosceles triangle has last two sides equal" $ do
            triangle (8, 6, 6) `shouldBe` Isosceles

        it "isosceles triangle has the first and the last sides equal" $ do
            triangle (6, 8, 6) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (6, 7, 8) `shouldBe` Scalene

        it "illegal triangle has sum of first two sides less or equal to the third one" $ do
            triangle (4, 5, 10) `shouldBe` Illegal
            triangle (4, 5, 9) `shouldBe` Illegal

        it "illegal triangle has sum of last two sides less or equal to the first one" $ do
            triangle (10, 5, 4) `shouldBe` Illegal
            triangle (9, 5, 4) `shouldBe` Illegal

        it "illegal triangle has sum of the first and the last sides less or equal to the second one" $ do
            triangle (5, 10, 4) `shouldBe` Illegal
            triangle (5, 9, 4) `shouldBe` Illegal

        it "illegal triangle has all sides equal to zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal
