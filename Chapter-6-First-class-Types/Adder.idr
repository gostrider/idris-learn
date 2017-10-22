module Adder

||| Return unary function type until Z
|||
||| AdderType Z       = Int
|||
||| AdderType S Z     = Int -> AdderType Z
|||
||| AdderType S (S Z) = Int -> AdderType (S Z)
|||
AdderType : (numargs : Nat) -> Type
AdderType Z     = Int
AdderType (S k) = (next : Int) -> AdderType k


adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k $ next + acc

||| Any numeric type
AdderType' : (numargs : Nat) -> Type -> Type
AdderType' Z numType     = numType
AdderType' (S k) numType = (next : numType) -> AdderType' k numType


-- adder (2 : numargs) (0 : acc) (2 : arg1) (2 : arg2) = 4
adder' : Num numType => (numargs : Nat) -> numType -> AdderType' numargs numType
adder' Z     acc = acc
adder' (S k) acc = \next => adder' k $ next + acc
