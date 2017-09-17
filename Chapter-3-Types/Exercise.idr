module Exercise

import Data.Vect

%default total

list_length : List a -> Nat
list_length [] = 0
list_length (x :: xs) = 1 + list_length xs


list_reverse : List a -> List a
list_reverse [] = []
list_reverse xs = foldl (\acc, x => x :: acc) [] xs


list_map : (a -> b) -> List a -> List b
list_map f [] = []
list_map f (x :: xs) = f x :: list_map f xs


vect_map : (a -> b) -> Vect n a -> Vect n b
vect_map f [] = []
vect_map f (x :: xs) = f x :: vect_map f xs
