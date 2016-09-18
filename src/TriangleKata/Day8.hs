module TriangleKata.Day8 (tests) where

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
            triangle (9, 9, 15) `shouldBe` Isosceles

        it "isosceles triangle has last two siedes equal" $ do
            triangle (14, 8, 8) `shouldBe` Isosceles

        it "isosceles triangle has the first and the last sides equal" $ do
            triangle (5, 6, 5) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (7, 8, 9) `shouldBe` Scalene

        it "illegal triangle has sum of first two sides less or equal to the third one" $ do
            triangle (5, 5, 11) `shouldBe` Illegal
            triangle (5, 5, 10) `shouldBe` Illegal

        it "illegal triangle has sum of last two sides less or equal to the first one" $ do
            triangle (12, 6, 5) `shouldBe` Illegal
            triangle (12, 6, 6) `shouldBe` Illegal

        it "illegal triangle has sum of the first and the last sides less or equal to the second one" $ do
            triangle (6, 13, 6) `shouldBe` Illegal
            triangle (6, 13, 7) `shouldBe` Illegal

        it "illegal triangle has all sides equal zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal
