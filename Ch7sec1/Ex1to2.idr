module Ex1to2

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle x z) (Triangle y w) = x==y && z==w
  (==) (Rectangle x z) (Rectangle y w) = x==y && z==w
  (==) (Circle x) (Circle y) = x==y
  (==) _ _ = False
  (/=) x y = not (x == y)

size: Shape -> Double
size (Triangle x y) = 0.5 * x * y
size (Rectangle x y) = x*y
size (Circle x) = pi * x * x

Ord Shape where
  compare x y = compare (size x) (size y)
--   (<) x y = ?Ord_rhs_2
--   (>) x y = ?Ord_rhs_3
--   (<=) x y = ?Ord_rhs_4
--   (>=) x y = ?Ord_rhs_5
--   max x y = ?Ord_rhs_6
--   min x y = ?Ord_rhs_7

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]