module DependentChurch
{- Parser for Church? -}


data Encoding = CBool Bool
              | CTrue Bool Bool
              | CFalse Bool Bool
              | CNot Encoding
              | CAnd Encoding Encoding
              | COr Encoding Encoding


andTerms : Bool -> Bool -> Encoding
andTerms True y = CBool y
andTerms x True = CBool x


orTerms : Bool -> Bool -> Encoding
orTerms x True = CBool True
orTerms True y = CBool True
orTerms _    _ = CBool False


eval : Encoding -> Bool
eval (CBool x)           = x
eval (CTrue x _)         = eval $ CBool x
eval (CFalse _ y)        = eval $ CBool y
eval (CNot (CNot x))     = eval x
eval (CNot (CTrue x y))  = eval $ CFalse x y
eval (CNot (CFalse x y)) = eval $ CTrue x y
eval (CAnd x y)          = eval $ andTerms (eval x) (eval y)
eval (COr x y)           = eval $ orTerms (eval x) (eval y)
