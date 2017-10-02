module Exercise_5_1


printLonger : IO ()
printLonger = do
  putStr "Enter first string: "
  input1 <- getLine
  putStr "Enter second string: "
  input2 <- getLine
  if (length input1) > (length input2)
  then putStrLn input1
  else putStrLn input2
