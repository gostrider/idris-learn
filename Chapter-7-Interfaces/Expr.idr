module Expr

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)


-- interface Cast from to where
--   cast : (orig : from) -> to

-- Expr.Cast (Maybe elem) (List elem) where
--   cast Nothing  = []
--   cast (Just x) = [x]


Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger


Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub


Abs ty => Abs (Expr ty) where
  abs = Abs


showOp : (Show a) => String -> a -> a -> String
showOp x y z = "(" ++ show y ++ x ++ show z ++ ")"


-- Exercise 7.2.1
Show ty => Show (Expr ty) where
  show (Val x)   = show x
  show (Add x y) = showOp "+" x y
  show (Sub x y) = showOp "-" x y
  show (Mul x y) = showOp "*" x y
  show (Div x y) = showOp "/" x y
  show (Abs x)   = show x


eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x)   = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x)   = abs $ eval x


-- Exercise 7.2.2
(Abs ty, Neg ty, Integral ty, Eq ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y
  (/=) x y = not $ eval x == eval y


-- Exercise 7.2.3
Cast (Expr ty) ToType where
  case from = ?holeTo
