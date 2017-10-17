module ProductsCoProducts

{-
c is better than c' if m : c' -> c
m factorizes p' & q'

        c'                   Int
   /    |    \          /     |     \
p'/     m     \q'    p /      m      \ q
 /      |      \      /       |       \
a <---- c ----> b   Int<-(Int, Bool)->Bool
    p       q          fst         snd

A product of a & b = c with 2 projections
such that for any other c' with 2 projections
there is a unique m : c' -> c
-}


fst' : (a, b) -> a
fst' (a, _) = a


snd' : (a, b) -> b
snd' (_, b) = b


-- m : c' -> c
m : a -> b -> (a, b)
m x y = (x, y)


-- p . m = a
p : (a, b) -> a
p (x, y) = fst' $ m x y


-- q . m = b
q : (a, b) -> b
q (x, y) = snd' $ m x y

-- m' : c -> c'
m' : (a, b) -> (a, b)
m' x = (p x, q x)


factorization : ((a, b) -> a) -> ((a, b) -> b) -> ((a, b) -> (a, b))
factorization p q x = (p x, q x)


-- CoProducts

-- i : a -> c
-- j : b -> c

-- i' = m . i
-- j' = m . j
