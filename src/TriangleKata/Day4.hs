module TriangleKata.Day4 (tests) where

    import Test.Hspec

    data TriangleType = Illegal | Equilateral | Isosceles | Scalene
        deriving (Eq, Show)

    type Triangle = (Int, Int, Int)

    triangle :: Triangle -> TriangleType
    triangle (0, 0, 0)      = Illegal
    triangle (a, b, c)
        | a + b <= c
            || b + c <= a
            || c + a <= b   = Illegal
        | a == b
            && b == c       = Equilateral
        | a == b
            || c == a
            || b == c       = Isosceles
        | otherwise         = Scalene

    tests = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle has first two sides equal" $ do
            triangle (6, 6, 10) `shouldBe` Isosceles

        it "isosceles triangle has last two sides equal" $ do
            triangle (7, 5, 5) `shouldBe` Isosceles

        it "isosceles triangle has the first and the last side equal" $ do
            triangle (8, 12, 8) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (5, 6, 7) `shouldBe` Scalene

        it "illegal triangle has all sides zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal

        it "illegal triangle has sum of first two sides less than the last one" $ do
            triangle (5, 5, 12) `shouldBe` Illegal

        it "illegal triangle has sum of last two sides less than the first one" $ do
            triangle (15, 3, 3) `shouldBe` Illegal

        it "illegal triangle has sum of the first and last sides less than the second one" $ do
            triangle (4, 8, 4) `shouldBe` Illegal
