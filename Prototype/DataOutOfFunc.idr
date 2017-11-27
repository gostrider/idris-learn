module DataOutOfFunc

{-
Church encoding for:

True  : \t.\f: t
False : \t.\f: f
-}

data Encoding = CBool Bool
              | CTrue Bool Bool
              | CFalse Bool Bool
              | CNot Encoding
              | CAnd Encoding Encoding
              | COr Encoding Encoding


andTerms : (x : Encoding) -> (y : Encoding) -> Encoding
andTerms x y = ?andTerms_rhs


orTerms : (x : Encoding) -> (y : Encoding) -> Encoding


eval : Encoding -> Bool
eval (CBool x)           = x
eval (CTrue x _)         = eval $ CBool x
eval (CFalse _ y)        = eval $ CBool y
eval (CNot (CNot x))     = eval x
eval (CNot (CTrue x y))  = eval $ CFalse x y
eval (CNot (CFalse x y)) = eval $ CTrue x y
eval (CAnd x y)          = eval $ andTerms x y
eval (COr x y)           = eval $ orTerms x y


-- CBool : forall r . r -> r -> r
-- CBool : Bool -> Bool -> Type
-- CBool _ _ = Bool


-- cTrue : (x : Bool) -> (y : Bool) -> CBool x y
-- cTrue x _ = x


-- cFalse : (x : Bool) -> (y : Bool) -> CBool x y
-- cFalse _ y = y


-- cNot : ((x : Bool) -> (y : Bool) -> CBool x y) -> (x : Bool) -> (y : Bool) -> CBool x y
-- cNot f x y = f (cFalse x y) (cTrue x y)


{-
call-by-value reducion

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
