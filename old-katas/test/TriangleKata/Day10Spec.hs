module TriangleKata.Day10Spec (spec) where

    import Test.Hspec
    import TriangleKata.Day10 (triangle, TriangleType(..))

    spec :: Spec
    spec = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle has first two sides equal" $ do
            triangle (7, 7, 10) `shouldBe` Isosceles

        it "isosceles triangle has last two sides equal" $ do
            triangle (10, 7, 7) `shouldBe` Isosceles

        it "isosceles triangle has the first and the last sides equal" $ do
            triangle (8, 11, 8) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (9, 10, 5) `shouldBe` Scalene

        it "illegal triangle has sum of first two sides less or equal to the third one" $ do
            triangle (1, 9, 10) `shouldBe` Illegal
            triangle (2, 7, 10) `shouldBe` Illegal

        it "illegal triangle has sum of last two sides less or equal to the first one" $ do
            triangle (12, 5, 7) `shouldBe` Illegal
            triangle (12, 5, 6) `shouldBe` Illegal

        it "illegal triangle has sum of the first and the last sides less or equal to the second one" $ do
            triangle (5, 11, 6) `shouldBe` Illegal
            triangle (5, 10, 5) `shouldBe` Illegal

        it "illegal triangle has all sides equal to zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal
