module Matrix

import Data.Vect


createEmpties : Vect n (Vect 0 e)
createEmpties = replicate _ []


-- transposeHelper : Vect n e -> Vect n (Vect len e) -> Vect n (Vect (S len) e)
-- transposeHelper [] [] = []
-- transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys


transposeMat : Vect m (Vect n e) -> Vect n (Vect m e)
transposeMat [] = createEmpties
transposeMat (x :: xs) = zipWith (\x1, y1 => x1 :: y1) x $ transposeMat xs


addMatrix : Num e => Vect rows (Vect cols e) -> Vect rows (Vect cols e) -> Vect rows (Vect cols e)
addMatrix [] _ = []
addMatrix (x :: xs) (y :: ys) = let joinRow = pairwiseAdd x y
                                in joinRow :: addMatrix xs ys
  where
    pairwiseAdd : Num e => Vect n e -> Vect n e -> Vect n e
    pairwiseAdd [] _ = []
    pairwiseAdd (x :: xs) (y :: ys) = x + y :: pairwiseAdd xs ys


dot : Num e => Vect n e -> Vect n e -> e
dot [] _ = 0
dot (x :: xs) (y :: ys) = x * y + dot xs ys


multMatrix' : Num e => Vect n (Vect m e) -> Vect p (Vect m e) -> Vect n (Vect p e)
multMatrix' [] _ = []
multMatrix' (x :: xs) ys = ((dot x) `map` ys) :: multMatrix' xs ys


multMatrix : Num e => Vect n (Vect m e) -> Vect m (Vect p e) -> Vect n (Vect p e)
multMatrix [] _ = []
multMatrix xs ys = multMatrix' xs $ transposeMat ys
