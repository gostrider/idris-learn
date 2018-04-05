module Exercise_7_3_2


data Vect : (n : Nat) -> (a : Type) -> Type where
  Nil : Vect 0 a
  (::) : a -> (xs : Vect k a) -> Vect (S k) a


(Eq a) => Eq (Vect n a) where
  [] == [] = True
  (x :: xs) == (y :: ys) = x == y && (xs == ys)


Foldable (Vect n) where
  foldr f acc [] = acc
  foldr f acc (x :: xs) = f x $ foldr f acc xs
