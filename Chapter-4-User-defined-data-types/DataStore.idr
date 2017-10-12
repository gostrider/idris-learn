module DataStore

import Data.Vect
import Data.Fin


data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit


data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore


sizeOf : DataStore -> Nat
sizeOf (MkData size _) = size


items : (store : DataStore) -> Vect (sizeOf store) String
items (MkData _ items') = items'


addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    -- Add item to the last
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items') = item :: addToData items'


parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args    = Just (Add args)
parseCommand "search" item = Just (Search item)
parseCommand "size" _      = Just Size
parseCommand "quit" _      = Just Quit
parseCommand "get" val     = case all isDigit (unpack val) of
                               False => Nothing
                               True  => Just (Get (cast val))
parseCommand _      _      = Nothing


parse : (input : String) -> Maybe Command
parse input = case Strings.span (/= ' ') input of
                (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let storeItems = items store in
    case integerToFin pos (sizeOf store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (index id storeItems ++ "\n", store)


-- member_of : (item : String) -> (store : DataStore) -> (m : Nat ** Vect m String)
-- member_of item (MkData size items') =
--   Vect.mapMaybe showItem items'
--     where
--       showItem : String -> Maybe String
--       showItem x = Just $ (finToNat x) ++ ": " ++ (index x store)
      -- (\x => Just (show (finToNat x) ++ ": " ++ (Vect.index x store)))


VectLength : (a -> Bool) -> Vect n a -> Nat
VectLength _ [] = 0
VectLength f (x :: xs) with (f x)
  | False = VectLength f xs
  | True = 1 + VectLength f xs


filter : (f : a -> Bool) -> (v : Vect n a) -> Vect (VectLength f v) a
filter _ [] = []
filter f (x :: xs) with (f x)
  | False = filter f xs
  | True = x :: filter f xs


-- Exercise 4.3.2 & 4.3.3
searchEntry : String -> DataStore -> Maybe (String, DataStore)
searchEntry item store =
  let
    matched = filter (Strings.isInfixOf item) (items store)
  in
    Just (showMatched matched, store)
      where
        showMatched : Vect n String -> String
        showMatched = foldr (\x, acc => x ++ "\n" ++ acc) ""


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse inp of
    Nothing              => Just ("Invalid Command\n", store)
    (Just (Add item))    => Just ("ID " ++ show (sizeOf store) ++ "\n", addToStore store item)
    (Just (Get pos))     => getEntry pos store
    (Just (Search item)) => searchEntry item store
    (Just Size)          => Just (show (sizeOf store) ++ "\n", store)
    (Just Quit)          => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
