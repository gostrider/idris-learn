module Exercise_5_2


data Guess = Higher Nat
           | Lower  Nat
           | Correct


data UserInput = Just String
               | Nothing


validInput : String -> UserInput
validInput input = if all isDigit $ unpack input
                   then Just input else Nothing


closeTo : Nat -> Nat -> Guess
closeTo guess value = case compare guess value of
                        LT => Higher guess
                        EQ => Correct
                        GT => Lower guess


display : Guess -> IO ()
display (Higher _) = putStrLn "Higher the guess"
display (Lower _)  = putStrLn "Lower the guess"
display Correct    = putStrLn "Correct"


guess : (target : Nat) -> IO ()
guess target = do userGuess <- getLine
                  Just input <- return $ validInput userGuess
                    | Nothing => putStrLn "Invalid input"
                  let guessResult = (cast input) `closeTo` target
                  display guessResult
                  Correct <- return guessResult
                    | _ => guess target
                  putStrLn "End"
