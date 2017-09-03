module Label

labelFrom : Integer -> List a -> List (Integer, a)
labelFrom label [] = []
labelFrom label (val :: vals) = (label, val) :: labelFrom (label + 1) vals


label : List a -> List (Integer, a)
label = labelFrom 0
