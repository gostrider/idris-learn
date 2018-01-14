module Peano


c0 : (Int -> Int) -> Int -> Int
c0 f = id


c1 : (Int -> Int) -> Int -> Int
c1 f = f


{-
(((+1) . (+1)) . ((+1) . (+1))) = 4
     c2 (+1)   .     c2 (+1)    = c2 (+1) 2
     f . f     .     f . f

Python:
     f   (f   (0)) === f . f
  (a)(c) ((b)(c) (d))
  (c2)(+1)((c2)(+1)(0))

-}
c2 : (Int -> Int) -> Int -> Int
c2 f = f . f


c3 : (Int -> Int) -> Int -> Int
c3 f = f . f . f


cSucc : ((Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int)
cSucc cn f = f . cn f


cPlus : ((Int -> Int) -> Int -> Int) ->
        ((Int -> Int) -> Int -> Int) ->
        ((Int -> Int) -> Int -> Int)
cPlus cm cn f = cm f . cn f

{-
cMult : (c2 . c2) (+1)  0
c2 (+1) $ c2 (+1)       0
          ((+1) . (+1)) 0

cMult c3 c2 (+1) 0 = f . f . f . f . f . f = 6
-}
cMult : ((Int -> Int) -> Int -> Int) ->
        ((Int -> Int) -> Int -> Int) ->
        ((Int -> Int) -> Int -> Int)
cMult cm cn = cm . cn


-- cExp : ((Int -> Int) -> Int -> Int) ->
--        ((Int -> Int) -> Int -> Int) ->
--        ((Int -> Int) -> Int -> Int)
-- cExp cm cn f z = cm cn f z


{-
c0 = lambda s: lambda z: z

lambda n: (Int -> Int) -> Int
lambda s: (+1)
lambda z: 0

succ            = lambda n: lambda s: lambda z: s( (n)(s)(z) )
succ(c0)        = lambda s: lambda z: s( c0(s)(z) )
succ(c0)(+1)    = lambda z: (+1)( c0(+1)(z) )
succ(c0)(+1)(0) = (+1)( c0(+1)(0) )
                = c1
(c1)        = lambda s: lambda z: s( succ(c0)(s)(z) )
(c1)(+1)    = lambda z: (+1)( succ(c0)(+1)(z) )
(c1)(+1)(0) = (+1)( succ(c0)(+1)(0) )
            = 2

Point free
c2 (+1) 0
c2 (+1) = (+1) . (+1)
-}
