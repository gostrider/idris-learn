module DataStore

import Data.Vect


data Command = Add String
             | Get Integer
             | Quit


data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore


size : DataStore -> Nat
size (MkData size' items') = size'


items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'


addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items') = item :: addToData items'


parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args = Just (Add args)
parseCommand "get" val  = case all isDigit (unpack val) of
                            False => Nothing
                            True => Just (Get (cast val))
parseCommand "quit" ""  = Just Quit
parseCommand _ _        = Nothing


parse : (input : String) -> Maybe Command
parse input = case Strings.span (/= ' ') input of
                (cmd, args) => parseCommand cmd (ltrim args)


processInput : DataStore -> String -> Maybe (String, DataStore)


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
