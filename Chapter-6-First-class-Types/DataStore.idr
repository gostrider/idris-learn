module DataStore

import Data.Vect
infixr 5 .+.


data Schema = SString
            | SChar
            | SInt
            | (.+.) Schema Schema




SchemaType : Schema -> Type
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType SString = String
SchemaType (x .+. y) = (SchemaType x, SchemaType y)




data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add       : SchemaType schema    -> Command schema
  Get       : Maybe Integer        -> Command schema
  Quit      :                         Command schema




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




parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString               input = getQuoted $ unpack input
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) =
      case span (/= '"') xs of
        (quoted, '"' :: rest) => Just (pack quoted, ltrim $ pack rest)
        _                     => Nothing
    getQuoted _           = Nothing

parsePrefix SChar                 input = case unpack input of
                                            c :: cs => Just (c, ltrim $ pack cs)
                                            []      => Nothing

parsePrefix SInt                  input = case span isDigit input of
                                            ("", rest)  => Nothing
                                            (num, rest) => Just (cast num, ltrim rest)

parsePrefix (schemal .+. schemar) input = do Just (l_val, input') <- pure $ parsePrefix schemal input
                                             Just (r_val, input'') <- pure $ parsePrefix schemar input'
                                             Just ((l_val, r_val), input'')




parseSchema : List String -> Maybe Schema
parseSchema ("String" :: []) = Just SString
parseSchema ("String" :: xs) = do Just xs_sch <- pure $ parseSchema xs
                                  Just (SString .+. xs_sch)

parseSchema ("Char"   :: []) = Just SChar
parseSchema ("Char"   :: xs) = do Just xs_sch <- pure $ parseSchema xs
                                  Just (SChar .+. xs_sch)

parseSchema ("Int"    :: []) = Just SInt
parseSchema ("Int"    :: xs) = do Just xs_sch <- pure $ parseSchema xs
                                  Just (SInt .+. xs_sch)

parseSchema _                = Nothing





parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _         => Nothing
                                  Nothing        => Nothing




parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" rest = do Just schema' <- pure $ parseSchema $ words rest
                                       Just (SetSchema schema')

parseCommand schema "add"    rest = do Just rest' <- pure $ parseBySchema schema rest
                                       Just (Add rest')

parseCommand schema "get"    ""   = Just (Get Nothing)
parseCommand schema "get"    val  = case all isDigit $ unpack val of
                                      False => Nothing
                                      True  => Just (Get $ Just $ cast val)

parseCommand schema "quit" _      = Just Quit
parseCommand _      _      _      = Nothing




parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = let (cmd, args) = span (/= ' ') input
                     in parseCommand schema cmd $ ltrim args




setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                           Z => Just $ MkData schema _ []
                           _ => Nothing




getAllEntry : Nat -> Vect size (SchemaType schema) -> String
getAllEntry idx [] = ""
getAllEntry idx (x :: xs) = show idx ++ ": " ++ display x ++ "\n" ++ getAllEntry (S idx) xs




||| Get item from data store
getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = do Just id <- pure $ integerToFin pos (size store)
                          | Nothing => Just ("Out of range\n", store)
                        Just (display (index id $ items store) ++ "\n", store)




processInput : (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Just Quit                => Nothing
    Just (Get (Just pos))    => getEntry pos store
    Just (Get Nothing)       => Just (getAllEntry Z $ items store, store)
    Just (Add item)          => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (SetSchema schema') => do Just store' <- pure $ setSchema store schema'
                                     | Nothing => Just ("Can't update schema\n", store)
                                   Just ("OK\n", store')
    Nothing                  => Just ("Invalid command\n", store)




main : IO ()
main = replWith (MkData (SString .+. SInt) _ []) "Command: " processInput
