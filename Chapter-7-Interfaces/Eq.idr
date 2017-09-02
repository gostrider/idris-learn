module MatterEq


data Matter = Solid | Liquid | Gas


interface MatterEq ty where
  (==) : ty -> ty -> Bool
  (/=) : ty -> ty -> Bool
  -- default method instance
  (==) x y = not (x /= y)
  (/=) x y = not (x == y)


MatterEq Matter where
  (==) Solid Solid   = True
  (==) Liquid Liquid = True
  (==) Gas Gas       = True
  (==) _   _         = False
  (/=) x y           = not (x == y)


occurrences : MatterEq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                  False => occurrences item xs
                                  True => 1 + occurrences item xs
