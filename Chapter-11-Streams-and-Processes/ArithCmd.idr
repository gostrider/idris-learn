module ArithCmd

import InfIO
import System
import ArithTotal

private
data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String


data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b


(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do


runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine


run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)

{-
Nested implementation

quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
quiz (num1 :: num2 :: nums) score = do
  PutStr ("Score so far: " ++ show score ++ "\n")
  PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
  answer <- GetLine
  if toLower answer == "quit" then Quit score else
    if (cast answer == num1 * num2)
      then do PutStr "Correct!\n"
              quiz nums (score + 1)
      else do PutStr ("Wrong, the answer is " ++ show (num1 * num2) ++ "\n")
              quiz nums score
-}

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do PutStr "Correct!\n"
                          quiz nums (score + 1)


  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans score = do PutStr ("Wrong, the ans is " ++ show ans ++ "\n")
                            quiz nums score


  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score = do
    PutStr ("Score so far: " ++ show score ++ "\n")
    PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
    answer <- GetLine
    if toLower answer == "quit" then Quit score else
      if (cast answer == num1 * num2)
        then correct nums score
        else wrong nums (num1 * num2) score


partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
            | Nothing => putStrLn "Out of fuel"
          putStrLn ("Final score: " ++ show score)
