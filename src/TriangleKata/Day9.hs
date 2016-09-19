module TriangleKata.Day9 (tests) where

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
            && c == a       = Equilateral
        | a == b
            || b == c
            || c == a       = Isosceles
        | otherwise         = Scalene

    tests = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle has first two sides equal" $ do
            triangle (10, 10, 15) `shouldBe` Isosceles

        it "isosceles triangle has last two sides equal" $ do
            triangle (14, 10, 10) `shouldBe` Isosceles

        it "isosceles triangle has the first and the last sides equal" $ do
            triangle (14, 10, 14) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (10, 11, 12) `shouldBe` Scalene

        it "illegal triangle has sum of first two sides less or equal to the third one" $ do
            triangle (14, 14, 28) `shouldBe` Illegal
            triangle (14, 13, 28) `shouldBe` Illegal

        it "illegal triangle has sum of last two sides less or equal to the first one" $ do
            triangle (25, 10, 15) `shouldBe` Illegal
            triangle (25, 10, 14) `shouldBe` Illegal

        it "illegal triangle has sum of the first and the last sides less or equal to the second one" $ do
            triangle (6, 12, 6) `shouldBe` Illegal
            triangle (5, 12, 6) `shouldBe` Illegal

        it "illegal triangle has all sides equal to zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal
