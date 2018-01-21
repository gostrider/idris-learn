module Exercise_5_3

import Data.Vect


-- Exercise 5.3.1
partial
readToBlank : IO (List String)
readToBlank = do current <- getLine
                 if current == ""
                 then pure []
                 else do next <- readToBlank
                         pure (current :: next)


-- Exercise 5.3.2
readAndSave : IO ()
readAndSave = do
  batch <- readToBlank
  putStrLn "Please enter filename:"
  fname <- getLine
  writeFile fname $ Strings.unlines batch
  putStrLn "Completed"


-- Exercise 5.3.3
readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right content <- readFile filename
    | Left err => pure (_ ** [])
  pure (_ ** fromList $ words content)
