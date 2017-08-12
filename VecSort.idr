module VecSort

import Data.Vect

insert : Ord elem => (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          True => x :: y :: xs
                          False => y :: insert x xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                      insert x xsSorted
