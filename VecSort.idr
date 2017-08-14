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

vectSize : Vect n items -> Nat
vectSize {n} _ = n


sample : Vect 4 String
sample = the (Vect _ String) ["aa", "bb", "ba", "bc"]


member_of : Vect n String -> List (Fin n)
member_of = Data.Vect.findIndices (Strings.isInfixOf "a")


get_member : {n : Nat} -> (store : Vect n String) -> (idx : List (Fin n)) -> (m : Nat ** Vect m (Nat, String))
get_member {n} store idx = Vect.mapMaybe maybe_member (Vect.fromList idx)
  where
    maybe_member : (x : Fin n) -> Maybe (Nat, String)
    maybe_member x = Just (Data.Fin.finToNat x, Vect.index x store)


flatten : List String -> String
flatten [] = ""
flatten (x :: xs) = x ++ flatten xs
