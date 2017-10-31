module Exercise_6_2

import Data.Vect


||| Z  c = []
||| 1 = [[0, 0, 0]]
||| 2 = [[0, 0, 0], [0, 0, 0]]
Matrix : (row : Nat) -> (column : Nat) -> Type
Matrix row column = Vect row $ Vect column Double


testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]


||| Z       = ()
||| S  k    = (ty, ())
||| S (S k) = (ty, (ty, ()))
TupleVect : (n : Nat) -> Type -> Type
TupleVect Z _ = ()
TupleVect (S k) ty = (ty, TupleVect k ty)


testTupleVect : TupleVect 4 Nat
testTupleVect = (1, 2, 3, 4, ())
