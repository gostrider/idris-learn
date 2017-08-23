module DataStore

import Data.Vect


data Command = Add String
             | Get Integer
             | Size
             | Search String
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
parseCommand "add" args    = Just (Add args)
parseCommand "get" val     = case all isDigit (unpack val) of
                               False => Nothing
                               True  => Just (Get (cast val))
parseCommand "size" _      = Just Size
parseCommand "quit" _      = Just Quit
parseCommand "search" item = Just (Search item)
parseCommand _      _      = Nothing


parse : (input : String) -> Maybe Command
parse input = case Strings.span (/= ' ') input of
                (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (index id store_items ++ "\n", store)


member_of : (item : String) -> (store : DataStore) -> (m : Nat ** Vect m String)
member_of item (MkData size items') = Vect.mapMaybe (\x => Just (show (Data.Fin.finToNat x) ++ ": " ++ (Vect.index x store))) items'


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing           => Just ("Invalid Command\n", store)
                              (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos))  => getEntry pos store
                              (Just Size)       => Just (show (size store) ++ "\n", store)
                              (Just (Search item)) => Just ("", store)
                              (Just Quit)       => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
