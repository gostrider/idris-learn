module Church

{-
Church encoding for:

True  : \t.\f: t
False : \t.\f: f
-}


{- Primitive Obsession -}

tru : Bool -> Bool -> Bool
tru x _ = x


fls : Bool -> Bool -> Bool
fls _ y = y


not : (Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
not f x y = f (fls x y) (tru x y)


or : (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
or f g x y = f (tru x y) (g x y)


and : (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
and f g x y = f (g x y) (fls x y)


{-
pair : \f.\s.\b. b f s
fst : \p. p tru
snd : \p. p fls

fst(pair v w)
=  fst((\f.\s.\b. b f s) v w)
-> fst((\s.\b. b v s) w)
-> fst((\b. b v w))
=  (\p. p tru)(\b. b v w)
-> (\b. b v w)tru
-> tru v w
=  v
-}


-- Return a function that takes (a pair of cBool) then apply args on the returned projection
pair : (Bool -> Bool -> Bool) ->
       (Bool -> Bool -> Bool) ->
       ((Bool -> Bool -> Bool) -> Bool -> Bool -> Bool)
pair f g = \h, x, y => h (f x y) (g x y)


fst : ((Bool -> Bool -> Bool) -> Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
fst f x y = f tru x y


snd : ((Bool -> Bool -> Bool) -> Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
snd f x y = f fls x y


c0 : (Int -> Int) -> Int -> Int
c0 f = id


c1 : (Int -> Int) -> Int -> Int
c1 f = f


c2 : (Int -> Int) -> Int -> Int
c2 f = f . f


cSucc : ((Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int)
cSucc cn f = f . cn f





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
-}




































{- Escaping from Primitive Obsession -}


-- CBool : forall r . r -> r -> r
CBool : Bool -> Bool -> Type
CBool _ _ = Bool


cTru : (x : Bool) -> (y : Bool) -> CBool x y
cTru x _ = x


cFls : (x : Bool) -> (y : Bool) -> CBool x y
cFls _ y = y


cNot : ((x : Bool) -> (y : Bool) -> CBool x y) -> (x : Bool) -> (y : Bool) -> CBool x y
cNot f x y = f (cFls x y) (cTru x y)




cOr : ((x : Bool) -> (y : Bool) -> CBool x y) ->
      ((x : Bool) -> (y : Bool) -> CBool x y) ->
      (x : Bool) -> (y : Bool) ->
      CBool x y
cOr f g x y = f (cTru x y) (g x y)




cAnd : ((x : Bool) -> (y : Bool) -> CBool x y) ->
       ((x : Bool) -> (y : Bool) -> CBool x y) ->
       (x : Bool) -> (y : Bool) ->
       CBool x y
cAnd f g x y = f (g x y) (cFls x y)




cPair : ((x : Bool) -> (y : Bool) -> CBool x y) ->
        ((x : Bool) -> (y : Bool) -> CBool x y) ->
        (((x : Bool) -> (y : Bool) -> CBool x y) -> ((x : Bool) -> (y : Bool) -> CBool x y))
cPair f g = \h, x, y => h (f x y) (g x y)




cFst : (((x : Bool) -> (y : Bool) -> CBool x y) -> ((x : Bool) -> (y : Bool) -> CBool x y)) ->
       (x : Bool) -> (y : Bool) -> CBool x y
cFst f x y = f tru x y




cSnd : (((x : Bool) -> (y : Bool) -> CBool x y) -> ((x : Bool) -> (y : Bool) -> CBool x y)) ->
       (x : Bool) -> (y : Bool) -> CBool x y
cSnd f x y = f fls x y

















































{-
Appendix: call-by-value reducion

cTrue = lambda t: lambda f: t
cFalse = lambda t: lambda f: f
test = lambda l: lambda m: lambda n: (l)(m)(n)

test cTrue v w                    test cFalse v w
-> (\l.\m.\n. l m n) cTrue v w    -> (\l.\m.\n. l m n) cFalse v w
-> (\m.\n. cTrue m n)v w          -> (\m.\n. cFalse m n)v w
-> (\n. cTrue v n)w               -> (\n. cFalse v n)w
=  cTrue v w                      =  cFalse v w
-> (\t.\f.t) v w                  -> (\t.\f.f) v w
-> (\f.v) w                       -> (\f.w) v
=  v                              =  w
-}
