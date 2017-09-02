module remove


import Data.Vect


-- Cannot guarantee that value will appear in the vect,
-- such that, type n will not be satisfy
-- remove : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
-- remove value (x :: xs) = case decEq value x of
--                                   (Yes prf) => xs
--                                   (No contra) => x :: remove value xs

-- data Elem : a -> Vect k a -> Type where
--   Here : Elem x (x :: xs)
--   There : (later : Elem x xs) -> Elem x (y :: xs)


oneInVect : Elem 1 [1, 2, 3]
oneInVect = Here


maryInVect : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVect = There (There Here)


remove : (value : a) -> -- Target value
         (xs : Vect (S n) a) ->
         (prf : Elem value xs) -> -- Proof that value is in the vector
         Vect n a
remove x (x :: xs) Here = xs
-- if vect length is z, xs must be []
-- absurd : Uninhabited t => t -> a
remove {n = Z} value (x :: []) (There later) = absurd later
remove {n = (S k)} value (x :: xs) (There later) = x :: remove value xs later


-- implicit pass provide proof
remove_auto : (value : a) ->
              (xs : Vect (S n) a) ->
              {auto prf : Elem value xs} ->
              Vect n a
remove_auto value xs {prf} = remove value xs prf


remove_combine : (value : a) ->
                 (xs : Vect (S n) a) ->
                 {auto prf : Elem value xs} ->
                 Vect n a
remove_combine value (value :: xs) {prf = Here} = xs
remove_combine {n = Z} value (x :: []) {prf = There later} = absurd later
remove_combine {n = (S k)} value (x :: xs) {prf = There later} = x :: remove_combine value xs
