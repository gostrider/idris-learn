module Expr

import DataTypes


data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr


evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = ?evaluate_rhs_2
evaluate (Sub x y) = ?evaluate_rhs_3
evaluate (Mult x y) = ?evaluate_rhs_4


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe x Nothing = x
maxMaybe Nothing y = y
maxMaybe (Just x) (Just y) =
  case compare x y of
    LT => Just y
    EQ => Just x
    GT => Just x


testPic1 : Picture
testPic1 = Combine (Primitive $ Triangle 2 3)
                   (Primitive $ Triangle 2 4)


testPic2 : Picture
testPic2 = Combine (Primitive $ Rectangle 1 3)
                   (Primitive $ Circle 4)


biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive (Triangle x y)) = ?biggestTriangle_rhs_5
biggestTriangle (Combine pic pic1) = ?biggestTriangle_rhs_2
