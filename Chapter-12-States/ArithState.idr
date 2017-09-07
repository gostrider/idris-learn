module ArithState


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


runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind x f) = runCommand x >>= (\x' => runCommand $ f x')
runCommand GetRandom = ?runCommand_rhs_1
runCommand GetGameState = ?runCommand_rhs_2
runCommand (PutGameState x) = ?runCommand_rhs_3


run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry p = pure Nothing
run fuel (Quit y) = do pure (Just y)
run (More fuel) (Do c f) = runCommand c >>= (\c' => run fuel $ f c')


mutual
  correct : ConsoleIO GameState
  correct = ?correct_rhs

  wrong : Int -> ConsoleIO GameState
  wrong ans = ?wrong_rhs

  -- readInput : (prompt : String) -> Command Input

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
