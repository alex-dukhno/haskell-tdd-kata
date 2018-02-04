module TriangleKata.Day1Spec (spec) where

    import Test.Hspec
    import TriangleKata.Day1 (triangle, TriangleType(..))

    spec :: Spec
    spec = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle with first two sides equal" $ do
            triangle (5, 5, 6) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (11, 12, 13) `shouldBe` Scalene

        it "isosceles triangle with last two equal sides" $ do
            triangle (6, 5, 5) `shouldBe` Isosceles

        it "isosceles triangle with first and last sides equal" $ do
            triangle (5, 6, 5) `shouldBe` Isosceles

        it "illegal triangle all sides are zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal

        it "illegal triangle has sum of the first two sides less than the third side" $ do
            triangle (5, 5, 11) `shouldBe` Illegal

        it "illegal triangle has sum of the last two sides less than the first side" $ do
            triangle (16, 5, 5) `shouldBe` Illegal

        it "illegal triangle has sum of first and last sides less than the second side" $ do
            triangle (6, 20, 6) `shouldBe` Illegal
