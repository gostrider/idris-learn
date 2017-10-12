data MaybeInt = FInt Int
              | FBool Bool


optional : MaybeInt -> Maybe Int
optional (FInt x) = Just x
optional (FBool x) = Nothing


isFInt : (maybeInt : MaybeInt) -> Maybe (x : Int ** (optional maybeInt = Just x))
isFInt maybeInt with (optional maybeInt)
  | Nothing = Nothing
  | (Just x) = Just (x ** Refl)


data Parity : Nat -> Type where
  Even : Parity (n + n)
  Odd  : Parity (S (n + n))


-- plusSuccRightSucc : S(1 + 1) = 1 + S 1
helpEven : (n : Nat) -> Parity (S n + S n) -> Parity (S (S (plus n n)))
helpEven n x = rewrite plusSuccRightSucc n n in x


helpOdd : (n : Nat) -> Parity (S (S n + S n)) -> Parity (S (S (S (plus n n))))
helpOdd n x = rewrite plusSuccRightSucc n n in x


parity : (n : Nat) -> Parity n
parity Z = Even { n=Z }
parity (S Z) = Odd { n=Z }
parity (S (S k)) with (parity k)
  -- Proof: Parity (S n + S n) = Parity (S (S (n + n)))
  parity (S (S (n + n)))     | Even = helpEven n $ Even { n=(S n) }
  parity (S (S (S (n + n)))) | Odd  = helpOdd n $ Odd { n=(S n) }


natToBin : Nat -> List Bool
natToBin Z = Nil
natToBin k with (parity k) -- [optional] proof p
  natToBin (n + n)     | Even = False :: natToBin n -- here, p : Nothing = optional foo
  natToBin (S (n + n)) | Odd  = True :: natToBin n  -- here, p : Just x  = optional foo
