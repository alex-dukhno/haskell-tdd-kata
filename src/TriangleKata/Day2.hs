module TriangleKata.Day2 (tests) where

    import Test.Hspec

    data TriangleType = Illegal | Equilateral | Isosceles | Scalene
        deriving (Eq, Show)

    type Triangle = (Int, Int, Int)

    triangle :: Triangle -> TriangleType
    triangle (0, 0, 0)      = Illegal
    triangle (a, b, c)
        | a + b < c
            || b + c < a
            || c + a < b    = Illegal
        | a == b
            && b == c       = Equilateral
        | a == b
            || b == c
            || c == a       = Isosceles
        | otherwise         = Scalene

    tests = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle with first two sides equal" $ do
            triangle (4, 4, 6) `shouldBe` Isosceles

        it "isosceles triangle with last two sides equal" $ do
            triangle (8, 5, 5) `shouldBe` Isosceles

        it "isosceles triangle with first and last sides equal" $ do
            triangle (6, 7, 6) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (4, 5, 6) `shouldBe` Scalene

        it "illegal triangle all sides zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal

        it "illegal triangle has sum of the first two sides less than the third side" $ do
            triangle (5, 5, 11) `shouldBe` Illegal

        it "illegal triangle has sum of the last two sides less than the first side" $ do
            triangle (11, 5, 5) `shouldBe` Illegal

        it "illegal triangle has sum of first and last sides less than the second side" $ do
            triangle (5, 11, 5) `shouldBe` Illegal

        it "illegal triangle has at least one side with negative length" $ do
            triangle (-1, 5, 7) `shouldBe` Illegal
