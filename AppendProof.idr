module AppendProof


import Data.Vect


{-
append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ?append_rhs_1
append (x :: xs) ys = ?append_rhs_2

case 1: -- ([] : Vect 0 elem) -> (ys : Vect m elem) -> Vect (0 + m) elem reduce to Vect m elem

Expect:
  ?append_rhs_1: Vect m elem

Swap return argument Type
append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = ?append_rhs_1
append (x :: xs) ys = ?append_rhs_2

Expect:
  ?append_rhs_1 : Vect (plus m 0) elem

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
-- a case for proofing valid of returning ys
append [] ys = ?append_nil ys
-- a case for proofing valid of returning x :: apppend xs ys
append (x :: xs) ys = ?append_xs (x :: append xs ys)

sym : left = right -> right = left
plusZeroRightNeutral : (left : Nat) -> left + 0 = left
plusSuccRightSucc : (left : Nat) -> (right : Nat) -> S (left + right) = left + S right

-}


append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs


append_xs : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
append_xs {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs


append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)


append_ori : Vect n elem -> Vect m elem -> Vect (n + m) elem
append_ori [] ys = ys
append_ori (x :: xs) ys = x :: append_ori xs ys
