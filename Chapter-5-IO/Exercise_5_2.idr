module Exercise_5_2

import System


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


guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStrLn ("Trial: " ++ show guesses)
  userGuess <- getLine
  Just input <- pure $ validInput userGuess
    | Nothing => putStrLn "Invalid input"
  let guessResult = (cast input) `closeTo` target
  display guessResult
  Correct <- pure guessResult
    | _ => guess target (S guesses)
  putStrLn "End"


-- Exercise 5.2.3
main : IO ()
main = do
  target <- time
  guess (fromInteger $ target `mod` 101) 0


-- Exercise 5.2.4
repl' : String -> (String -> String) -> IO ()
repl' msg fn = do
  putStrLn msg
  input <- getLine
  putStrLn $ fn input
  repl' msg fn


replWith' : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
replWith' state msg fn = do
  putStrLn msg
  input <- getLine
  case fn state input of
        Nothing => pure ()
        Just (result, state') => do
          putStrLn ("Result: " ++ result)
          replWith' state' result fn
