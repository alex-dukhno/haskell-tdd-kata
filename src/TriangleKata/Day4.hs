module TriangleKata.Day4 (triangle, TriangleType(..)) where


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
