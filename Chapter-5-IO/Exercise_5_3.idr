module Exercise_5_3

import Data.Vect


partial
readToBlank : IO (List String)
readToBlank = do current <- getLine
                 if current == ""
                 then pure []
                 else do next <- readToBlank
                         pure (current :: next)


readAndSave : IO ()
readAndSave = do
  batch <- readToBlank
  putStrLn "Please enter filename:"
  fname <- getLine
  writeFile fname $ Strings.unlines batch
  putStrLn "Completed"


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right content <- readFile filename
    | Left err => pure (_ ** [])
  pure (_ ** fromList $ words content)
