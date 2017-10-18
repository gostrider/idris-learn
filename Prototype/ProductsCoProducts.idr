module ProductsCoProducts

{-

c is better than c' if m : c' -> c
m factorizes p' & q'

      c'                          Int
   /  |  \                 /       |       \
p'/   m   \ q'         p' /        m        \ q'
 /    |    \             /         |         \
a <---c---> b         Int <---(Int, Bool)---> Bool
    p   q                  fst           snd

A product of a & b = c with 2 projections
such that for any other c' with 2 projections
there is a unique m : c' -> c

-}

m : a -> (a, Bool)
m x = (x, True)


fst' : (a, b) -> a
fst' (a, _) = a


snd' : (a, b) -> b
snd' (_, b) = b


p : a -> a
p = fst' . m


q : a -> Bool
q = snd' . m


m' : a -> (a, Bool)
m' x = (p x, q x)


factorization : (a -> a) -> (a -> b) -> (a -> (a, b))
factorization p q x = (p x, q x)

{-

     i   j                fst               snd
 a---> c <---b         Int---> (Int, Bool) <---Bool
  \    |    /             \         |         /
 i'\   m   / j'          i'\        m        / j'
    \  |  /                 \       |       /
       c'                          Int

-}


i' : a -> (a, Bool)
-- i' = m . i
-- j' = m . j
