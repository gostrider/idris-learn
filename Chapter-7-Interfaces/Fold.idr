module Fold


data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)


interface Foldable (t: Type -> Type) where
  foldr : (elem -> acc -> acc) -> acc -> t elem -> acc
  foldl : (acc -> elem -> acc) -> acc -> t elem -> acc


Fold.Foldable List where
  foldr f acc [] = acc
  foldr f acc (x :: xs) = f x (Fold.foldr f acc xs)

  foldl f acc [] = acc
  foldl f acc (x :: xs) = Fold.foldl f (f acc x) xs


Fold.Foldable Tree  where
  foldr f acc Empty = ?rhs_1
  foldr f acc (Node left e right) =
    let leftfold = foldr f acc left
        rightfold = foldr f leftfold right
    in f e rightfold


totalLen : List String -> Nat
totalLen xs = Fold.foldr addLen 0 xs
  where
    addLen : String -> Nat -> Nat
    addLen x k = length x + k
