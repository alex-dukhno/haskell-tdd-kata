module TriangleKata.Day3Spec (spec) where

    import Test.Hspec
    import TriangleKata.Day3 (triangle, TriangleType(..))

    spec :: Spec
    spec = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle has first two sides equal" $ do
            triangle (6, 6, 9) `shouldBe` Isosceles

        it "isosceles triangle has last two sides equal" $ do
            triangle (9, 6, 6) `shouldBe` Isosceles

        it "isosceles triangle has the first and the last sides equal" $ do
            triangle (7, 8, 7) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (5, 6, 7) `shouldBe` Scalene

        it "illegal triangle has all sides equal zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal

        it "illegal triangle has sum of first two sides less than the third side" $ do
            triangle (5, 5, 11) `shouldBe` Illegal

        it "illegal triangle has sum of last two sides less than the first side" $ do
            triangle (11, 5, 5) `shouldBe` Illegal

        it "illegal triangle has sum of the first and the last sides less than the second side" $ do
            triangle (5, 11, 5) `shouldBe` Illegal
