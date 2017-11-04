module DataStore

import Data.Vect
infixr 5 .+.

%default total

data Schema = SString
            | SChar
            | SInt
            | (.+.) Schema Schema




SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SChar = Char
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)




data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add  : SchemaType schema -> Command schema
  Get  : Integer           -> Command schema
  Quit :                      Command schema




||| Automatically generate projection functions
record DataStore where
  constructor MkData
  schema : Schema
  size   : Nat
  items  : Vect size (SchemaType schema)




||| Add new item to data store
addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema _ store) newItem = MkData schema _ (addToData store)
  where
    addToData : Vect k (SchemaType schema) -> Vect (S k) (SchemaType schema)
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items




||| Display element
display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SChar} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr




||| Get item from data store
getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store
  in case integerToFin pos (size store) of
      Nothing => Just ("out of range\n", store)
      Just id => Just (display (index id $ items store) ++ "\n", store)




partial
parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted $ unpack input
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                              (quoted, '"' :: rest) => Just (pack quoted, ltrim $ pack rest)
                              _                     => Nothing
    getQuoted _           = Nothing

parsePrefix SChar input = case Strings.unpack input of
                            [c] => Just (c, "")
                            _ => Nothing

parsePrefix SInt input = case span isDigit input of
                           ("", rest)  => Nothing
                           (num, rest) => Just (cast num, ltrim rest)

parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                            Nothing              => Nothing
                                            Just (l_val, input') =>
                                              case parsePrefix schemar input' of
                                                Nothing               => Nothing
                                                Just (r_val, input'') => Just ((l_val, r_val), input'')




partial
parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _         => Nothing
                                  Nothing        => Nothing




parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                 [] => Just SString
                                 _  => case parseSchema xs of
                                         Nothing     => Nothing
                                         Just xs_sch => Just (SString .+. xs_sch)

parseSchema ("Char" :: xs)   = case xs of
                                    [] => Just SChar
                                    _ => case parseSchema xs of
                                              Nothing => Nothing
                                              Just xs_sch => Just (SChar .+. xs_sch)

parseSchema ("Int" :: xs)    = case xs of
                                 [] => Just SInt
                                 _  => case parseSchema xs of
                                         Nothing     => Nothing
                                         Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _                = Nothing




partial
parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" rest = case parseSchema $ words rest of
                                      Nothing      => Nothing
                                      Just schema' => Just (SetSchema schema')

parseCommand schema "add" rest    = case parseBySchema schema rest of
                                      Nothing    => Nothing
                                      Just rest' => Just (Add rest')

parseCommand schema "get" val     = case all isDigit $ unpack val of
                                      False => Nothing
                                      True  => Just (Get $ cast val)

parseCommand schema "quit" _      = Just Quit
parseCommand _      _      _      = Nothing




partial
parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case Strings.span (/= ' ') input of
                       (cmd, args) => parseCommand schema cmd $ ltrim args




setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z   => Just (MkData schema _ [])
                              S k => Nothing




partial
processInput : (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Just Quit                => Nothing
    Just (Get pos)           => getEntry pos store
    Nothing                  => Just ("Invalid command\n", store)
    Just (Add item)          => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (SetSchema schema') => case setSchema store schema' of
                                      Nothing     => Just ("Can't update schema\n", store)
                                      Just store' => Just ("OK\n", store')




partial
main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput