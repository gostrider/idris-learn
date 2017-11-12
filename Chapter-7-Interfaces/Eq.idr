module MatterEq


data Matter = Solid | Liquid | Gas


data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)


%name Tree left ,value, right


interface MatterEq ty where
  (==) : ty -> ty -> Bool
  (/=) : ty -> ty -> Bool

  {- default method instance
     used if the method isn't
     present in a specific impl.
     of an interface.
  -}
  x == y = not $ x /= y
  x /= y = not $ x == y


MatterEq Matter where
  (==) Gas    Gas    = True
  (==) Solid  Solid  = True
  (==) Liquid Liquid = True
  (==) _      _      = False

  (/=) x      y      = not $ x == y


MatterEq elem => MatterEq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left x right) (Node left' y right') =
    left == left' && x == y && right == right'
  (==) _ _ = False

  (/=) x y = ?MatterEq_rhs_2


-- occurrences Solid [Solid, Gas, Liquid, Solid]
occurrences : MatterEq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) =
  case x == item of
    False => occurrences item xs
    True => 1 + occurrences item xs
