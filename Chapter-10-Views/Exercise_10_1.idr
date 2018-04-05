module Exercise_10_1

data TakeN : List a -> Type where
  -- Element list less then N elements
  Fewer : TakeN xs
  -- Element list match N elements
  Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)


total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S _) (_ :: _) | Fewer = Fewer
  takeN (S _) (x :: (n_xs ++ _)) | Exact _ = Exact (x :: n_xs)


groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n [] | Fewer = []
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest
