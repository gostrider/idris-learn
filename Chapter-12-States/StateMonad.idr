module StateMonad

-- import Control.Monad.State


interface Applicative m => Monad (m : Type -> Type) where
  (>>=) : m a -> (a -> m b) -> m b
  join  : m (m a) -> m a


interface Functor (f : Type -> Type) where
  map : (func : a -> b) -> f a -> f b


interface StateMonad.Functor f => APplicative (f : Type -> Type) where
  pure : a -> f a
  (<*>) : f (a -> b) -> f a -> f b


addIfPositive : Integer -> State Integer Bool
addIfPositive val =
  do when (val > 0) (do current <- get
                        put (current + val))
     pure (val > 0)


addPositives : List Integer -> State Integer Nat
addPositives vals = do added <- traverse addIfPositive vals
                       pure $ length $ filter id added
