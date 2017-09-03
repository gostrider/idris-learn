module RunIO

import InfIO


data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b


(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do


greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
             then do putStrLn "Bye"
                     Quit ()
             else do putStrLn ("hello " ++ name)
                     greet


run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit value) = pure (Just value)
run Dry p = pure Nothing
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
