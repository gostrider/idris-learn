module Tree

data Tree elem = Empty
							 | Node (Tree elem) elem (Tree elem)


{-
data BSTree : Type -> Type where
	Empty : Ord elem => BSTree elem
	Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem


insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)
-}


insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x ori@(Node left val right) =
	case compare x val of
		LT => Node (insert x left) val right
		EQ => ori
		GT => Node left val (insert x right)


listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (l :: xs) = insert l $ listToTree xs


treeToList : Ord elem => Tree elem -> List elem
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ x :: treeToList right
