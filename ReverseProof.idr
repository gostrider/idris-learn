module ReverseProof


import Data.Vect


{-
myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x] -- Type error

Expect:
  Vect (S k) elem, but Vect (k + 1) elem was given.

idris> \k, elem => Vect (1 + k) elem
\k, elem => Vect (S k) elem : Nat -> Type -> Type

idris> \k, elem => Vect (k + 1) elem
\k, elem => Vect (plus k 1) elem : Nat -> Type -> Type

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) =
  let rev_xs = myReverse xs
      result = rev_xs ++ [x]
  in
    ?myReverse_rhs_2

?myReverse_rhs_2 : Vect (S k) elem -- what the program expect
theefore, we have to specify that 1 + k == k + 1,
that, S k == plus k 1

plusCommutative : (left : Nat) -> (right : Nat) -> left + right = right + left
idris>:t \k => plusCommutative 1 k
\k => plusCommutative 1 k : (k : Nat) -> S k = plus k 1

-}

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) =
  let rev_xs = myReverse xs
      result = rev_xs ++ [x]
  in
    -- Rewirte rule with type S k = plus k 1
    rewrite plusCommutative 1 k
    -- Rewrite rule replaces S k with plus k 1 in this hole
    in result


myReverse2 : Vect n elem -> Vect n elem
myReverse2 [] = []
myReverse2 (x :: xs) = reverseProof (myReverse2 xs ++ [x])
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} xs = rewrite plusCommutative 1 k
                          in xs
