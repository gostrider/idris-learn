module ListView


describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty, tail= " ++ show xs

{-

When checking left hand side of describeListEnd:
Can't match on describeListEnd (xs ++ [x])

describeListEnd : List Int -> String
describeListEnd [] = "Empty"
describeListEnd (xs ++ [x]) = "Non-empty, initial portion= " ++ show xs
-}

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])


total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          (NonEmpty ys y) => NonEmpty (x :: ys) y

{-
describeHelper : (input : List Int) -> ListLast input -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion= " ++ show xs


describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)
-}


-- Using with view
describeListEnd : List Int -> String
describeListEnd input with (listLast input)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (xs ++ [x]) | (NonEmpty xs x) = "Non-empty, initial portion= " ++ show xs


-- This function is not total
myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs
