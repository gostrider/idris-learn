module ApplicativeStyle

record Person where
  constructor MkPerson
  name : String
  lastName : String

data Error = String


validateName : String -> Either String String
validateName "test" = Right "test"
validateName _      = Left "Invalid name"


validateLastName : String -> Either String String
validateLastName "runnable" = Right "runnable"
validateLastName _          = Left "Invalid last name"


validPersonM : String -> String -> Either String Person
validPersonM name lastName = do
  vName <- validateName name
  vLast <- validateLastName lastName
  pure $ MkPerson vName vLast


{- When do block can be substituted by liftM2/3/4
   When operations does not depend on each other
-}
validPersonA : String -> String -> Either String Person
validPersonA name lastName =
  MkPerson <$> validateName name <*> validateLastName lastName
