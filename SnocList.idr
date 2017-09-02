module SnocList

data SnocList : List a -> Type where
	Empty : SnocList []
	Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])


snocListHelper :

snocList : (xs : List a) -> SnocList xs


myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec


myReverse : List a -> List a
myReverse xs = myReverseHelper xs (snocList xs)
