readPair : IO (String, String)
readPair = do
  str1 <- getLine
  str2 <- getLine
  pure (str1, str2)


usePair : IO ()
usePair = do
  (str1, str2) <- readPair
  putStrLn ("You entered " ++ str1 ++ " and " ++ str2)


readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit $ unpack input
  then pure $ Just $ cast input
  else pure Nothing


readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  Just num1 <- readNumber | Nothing => pure Nothing
  Just num2 <- readNumber | Nothing => pure Nothing
  pure $ Just (num1, num2)
