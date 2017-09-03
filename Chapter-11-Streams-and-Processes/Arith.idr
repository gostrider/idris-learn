module Arith

import Data.Primitives.Views


randoms : Int -> Stream Int
randoms seed =
  let seed' = 1663425 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'


arithInputs : Int -> Stream Int
arithInputs seed = map ?bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1


quiz : Stream Int -> (score : Nat) -> IO ()
quiz (num1 :: num2 :: nums) score = do
  putStrLn ("Score: " ++ show score)
  putStr (show num1 ++ " * " ++ show num2 ++ "? ")
  answer <- getLine
  if cast answer == num1 * num2
    then do putStrLn "Correct"
            quiz nums (score + 1)
    else do putStrLn ("Wrong, the answer is " ++ show (num1 * num2))
            quiz nums score
