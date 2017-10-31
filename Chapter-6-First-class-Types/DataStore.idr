module DataStore

import Data.Vect

infixr 5 .+.


data Schema = SString
            | SInt
            | (.+.) Schema Schema


SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

{-
Normal data constructor, tedious on writing projection functions

data DataStore : Type where
  MkData : (schema : Schema)                        ->
           (size   : Nat)                           ->
           (items  : Vect size (SchemaType schema)) ->
           DataStore


size : DataStore -> Nat
size (MkData _ size' _) = size'


schema : DataStore -> Schema
schema (MkData schema' _ _) = schema'


items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData _ _ items') = items'
-}

||| Automatically generate projection functions
record DataStore where
  constructor MkData
  schema : Schema
  size   : Nat
  items  : Vect size (SchemaType schema)


addToStore : DataStore -> String -> DataStore
addToStore (MkData schema size items) newItem = ?addToStore_rhs_1
