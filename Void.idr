module Void

twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible


valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible


-- Zero is not Successor of Number
zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible


-- Successor of Number is not Zero
sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible


-- Given a proof of two number are not equal
-- and a proof of their siccessors are equal,
-- produce a value of the empty type.
-- contra tells k is guaranteed not equal to j
noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

-- Prelude interface decEq : (val1 : ty) -> (val2 : ty) -> Dec (val1 = val2)
checkEqNat : (num1: Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              (Yes prf) => Yes (cong prf)
                              (No contra) => No (noRec contra)
