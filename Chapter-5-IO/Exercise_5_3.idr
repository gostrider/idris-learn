module Exercise_5_3

import Data.Vect


partial
readToBlank : IO (List String)
readToBlank = do current <- getLine
                 if current == ""
                 then pure []
                 else do next <- readToBlank
                         pure (current :: next)


writeLines : String -> List String -> IO (Either FileError ())
writeLines file [] = pure(Right ())
writeLines file (line :: lines) = do writeFile file line
                                     writeLines file lines


readAndSave : IO ()
readAndSave = do
  batch <- readToBlank
  putStrLn "Please enter filename:"
  fname <- getLine
  writeLines fname batch
  putStrLn "Completed"


readVectFile : (filename : String) -> IO (n ** Vect n String)
