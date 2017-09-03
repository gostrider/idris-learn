module DataStore

import Data.Vect

infixr 5 .+.

public export
data Schema = SString
            | SInt
            | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)


export
record DataStore (schema : Schema) where
