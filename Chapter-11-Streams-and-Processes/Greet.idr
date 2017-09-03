module Greet


-- No termination specified
greet : InfIO
greet = do
  putStr "Enter your name: "
  name <- getLine
  putStrLn ("hello " ++ name)
  greet
