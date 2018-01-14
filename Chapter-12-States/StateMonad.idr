module StateMonad

-- import Control.Monad.State


interface Applicative m => Monad (m : Type -> Type) where
  (>>=) : m a -> (a -> m b) -> m b
  join  : m (m a) -> m a


interface Functor (f : Type -> Type) where
  map : (func : a -> b) -> f a -> f b


interface StateMonad.Functor f => Applicative (f : Type -> Type) where
  pure : a -> f a
  (<*>) : f (a -> b) -> f a -> f b

mutual
  Functor (State stateType) where
    map f x = do x' <- x
                 Pure (f x')


  Applicative (State stateType) where
    pure = Pure
    (<*>) f a = do f' <- f
                   a' <- a
                   pure (f' a')


  Monad (State stateType) where
    (>>=) = Bind


addIfPositive : Integer -> State Integer Bool
addIfPositive val =
  do when (val > 0) (do current <- get
                        put (current + val))
     pure (val > 0)


addPositives : List Integer -> State Integer Nat
addPositives vals = do added <- traverse addIfPositive vals
                       pure $ length $ filter id added
