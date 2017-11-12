module FreeApplicative

data FAp : (f : Type -> Type) -> a -> Type where
  Pure : a -> FAp f a
  Ap : f a -> FAp f (a -> b) -> FAp f b


Functor (FAp f) where
  map f (Pure x) = Pure $ f x
  map f (Ap x y) = Ap x $ (f .) <$> y


Applicative (FAp f) where
  pure = Pure
  (Pure f') <*> x  = map f' x
  (Ap x f') <*> x' = Ap x $ flip <$> f' <*> x'
