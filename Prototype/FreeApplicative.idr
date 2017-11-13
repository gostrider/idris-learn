module FreeApplicative


public export
data Free : (f : a -> b) -> a -> Type where
  Pure : a -> Free f a
  Ap : f a -> Free f (a -> b) -> Free f b


public export
Functor f => Functor (Free f) where
  map f (Pure x) = Pure $ f x
  map f (Ap x y) = Ap x $ (f .) <$> y


public export
Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) (Pure f') x  = map f' x
  (<*>) (Ap x f') x' = Ap x $ flip <$> f' <*> x'
