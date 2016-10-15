module TriangleKata.Day6 (triangle, TriangleType(..)) where

    data TriangleType = Illegal | Equilateral | Isosceles | Scalene
        deriving(Eq, Show)

    type Triangle = (Int, Int, Int)

    triangle :: Triangle -> TriangleType
    triangle (a, b, c)
        | a + b <= c
            || b + c <= a
            || c + a <= b   = Illegal
        | a == b
            && b == c       = Equilateral
        | a == b
            || b == c
            || c == a       = Isosceles
        | otherwise         = Scalene
