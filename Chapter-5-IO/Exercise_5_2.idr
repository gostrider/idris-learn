module Exercise_5_2


data Guess = Higher Nat
           | Lower  Nat
           | Correct


data VerifyInput = Valid
                 | Invalid


validInput : String -> VerifyInput
validInput input = if all isDigit $ unpack input
                   then Valid else Invalid


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
                  case validInput userGuess of
                    Invalid => putStrLn "Invalid input"
                    Valid => do let guessResult = closeTo (cast userGuess) target
                                display guessResult
                                case guessResult of
                                      Correct => putStrLn "End"
                                      _ => guess target
