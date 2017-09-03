module InfList


-- Aka streams
data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem


%name InfList xs, ys, zs


-- Identical to repeat (x + 1)
countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)


-- Identical to take
getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs


labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith labels [] = []
labelWith (label :: labels) (val :: vals) = (label, val) :: labelWith labels vals


label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)
