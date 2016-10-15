module TriangleKata.Day5Spec (spec) where

    import Test.Hspec
    import TriangleKata.Day5 (triangle, TriangleType(..))

    spec :: Spec
    spec = do
        it "equilateral triangle has all sides equal" $ do
            triangle (10, 10, 10) `shouldBe` Equilateral

        it "isosceles triangle has first two sides equal" $ do
            triangle (6, 6, 8) `shouldBe` Isosceles

        it "isosceles triangle has last two sides equal" $ do
            triangle (8, 6, 6) `shouldBe` Isosceles

        it "isosceles triangle has the first and the last sides equal" $ do
            triangle (4, 6, 4) `shouldBe` Isosceles

        it "scalene triangle has no equal sides" $ do
            triangle (4, 7, 8) `shouldBe` Scalene

        it "illegal triangle has all sides zero" $ do
            triangle (0, 0, 0) `shouldBe` Illegal

        it "illegal triangle has sum of first two sides less or equal than the third one" $ do
            triangle (4, 5, 10) `shouldBe` Illegal
            triangle (4, 5, 9) `shouldBe` Illegal

        it "illegal triangle has sum of last two sides less or equal than the third one" $ do
            triangle (10, 4, 4) `shouldBe` Illegal
            triangle (9, 5, 4) `shouldBe` Illegal

        it "illegal triangle has sum of the first and the last sides less or equal than the second one" $ do
            triangle (4, 10, 4) `shouldBe` Illegal
            triangle (2, 4, 2) `shouldBe` Illegal
