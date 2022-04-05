module Baby where

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmall x =
  if x > 100
    then x
    else x * 2

filterEven xxs =
  [[x | x <- xs, even x] | xs <- xxs]

rightTriangles perimeter =
  [ (a, b, c) | c <- [1 .. perimeter], a <- [1 .. c], b <- [1 .. a], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == perimeter
  ]