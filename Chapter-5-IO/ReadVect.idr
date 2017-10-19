module ReadVect

import Data.Vect

%default total


||| Encapsulate vector length within the type.
|||
||| User does not have to know the length when constructing the type.
||| ```idris example
||| MkVect _ ["aaa", "bbb", "ccc"]
||| ```
data VectUnknown : Type -> Type where
  ||| Type constructors
  ||| @ len The length of the Vect
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a


readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)


partial
readVect : IO (VectUnknown String)
readVect = do x <- getLine
              if x == ""
              then pure $ MkVect _ []
              else do MkVect _ xs <- readVect
                      pure $ MkVect _ (x :: xs)


partial
readDPairVect : IO (len ** Vect len String)
readDPairVect = do x <- getLine
                   if x == ""
                   then pure (_ ** [])
                   else do (_ ** xs) <- readDPairVect
                           pure (_ ** x :: xs)


printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")


zipInputs : IO ()
zipInputs = do
  putStrLn "First vector: (end with blank line):"
  (len1 ** vect1) <- readDPairVect
  putStrLn "Second vector: (end with blank line):"
  (_ ** vect2) <- readDPairVect
  case exactLength len1 vect2 of
    Nothing => putStrLn "Both vector length do not match"
    Just vect2' => printLn (zip vect1 vect2')
