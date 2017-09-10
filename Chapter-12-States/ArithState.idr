module ArithState

import Data.Primitives.Views
import System


record Score where
  constructor MkScore
  correct : Nat
  attempted : Nat


record GameState where
  constructor MkGameState
  score : Score
  difficulty : Int


Show GameState where
  show st = show (correct (score st)) ++ "/" ++
            show (attempted (score st)) ++ "\n" ++
            "Difficulty: " ++ show (difficulty st)


data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

  GetRandom : Command Int
  GetGameState : Command GameState
  PutGameState : GameState -> Command ()

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b


data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b


namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind


namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do


data Fuel = Dry
          | More (Lazy Fuel)


-- Record projection functions


setDifficulty : Int -> GameState -> GameState
setDifficulty newDiff = record { difficulty = newDiff }


addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }


addCorrect : GameState -> GameState
addCorrect = record { score->correct   $= (+1)
                    , score->attempted $= (+1)
                    }


-- Quiz functions


initState : GameState
initState = MkGameState (MkScore 0 0) 12


forever : Fuel
forever = More forever


runCommand : Stream Int -> GameState -> Command a -> IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do putStr x
                                      pure ((), rnds, state)

runCommand rnds state GetLine = do str <- getLine
                                   pure (str, rnds, state)

runCommand (val :: rnds) state GetRandom =
  pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0 | DivByZero = 1
      getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1

runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)
runCommand rnds state (Pure val) = pure (val, rnds, state)
runCommand rnds state (Bind c f) = do
  (res, rnds', state') <- runCommand rnds state c
  runCommand rnds' state' $ f res


run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> IO (Maybe a, Stream Int, GameState)
run fuel rnds state (Quit x) = pure (Just x, rnds, state)
run fuel rnds state (Do c f) = do
  (x, rnds', state') <- runCommand rnds state c
  run fuel rnds' state' $ f x
run Dry rnds state p = pure (Nothing, rnds, state)


randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223
               in (seed' `shiftR` 2) :: randoms seed'


-- Quiz Logic

data Input = Answer Int
           | QuidCmd


readInput : (prompt : String) -> Command Input
readInput prompt = do
  PutStr prompt
  answer <- GetLine
  if toLower answer == "quit"
    then Pure QuidCmd
    else Pure (Answer (cast answer))


mutual
  correct : ConsoleIO GameState
  correct = do PutStr "Correct "
               st <- GetGameState
               PutGameState (addCorrect st)
               quiz

  wrong : Int -> ConsoleIO GameState
  wrong ans = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                 st <- GetGameState
                 PutGameState (addWrong st)
                 quiz

  quiz : ConsoleIO GameState
  quiz = do
    num1 <- GetRandom
    num2 <- GetRandom
    st <- GetGameState
    PutStr $ show st ++ "\n"
    input <- readInput $ show num1 ++ " * " ++ show num2 ++ "?"
    case input of
      Answer ans => if ans == num1 * num2 then correct else wrong (num1 * num2)
      QuitCmd => Quit st


partial
main : IO ()
main = do
  seed <- time
  (Just score, _, state) <- run forever (randoms (fromInteger seed)) initState quiz
    | _ => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show state)
