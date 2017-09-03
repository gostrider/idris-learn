module InfIO


data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO


loopPrint : String -> InfIO
--loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)
loopPrint msg = do putStrLn msg
                   loopPrint msg

{-

This function is not total

run : InfIO -> IO ()
run (Do action cont) = do res <- action
                          run (cont res)
-}

{-
Eager version of fuel
data Fuel = Dry
          | More Fuel
-}


data Fuel = Dry
          | More (Lazy Fuel)


forever : Fuel
forever = More forever


tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)


-- Still require to specify maximum number of action
run : Fuel -> InfIO -> IO ()
run Dry p = putStrLn "Out of fuel"
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
