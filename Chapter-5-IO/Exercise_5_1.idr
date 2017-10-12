module Exercise_5_1


larger : Nat -> Nat -> Nat
larger x y with (compare x y)
  | GT = x
  | _ = y


largerInput : String -> String -> Nat
largerInput x y = larger (length x) (length y)


printBind : IO ()
printBind =
  putStr "Enter first string: " >>=
    \_ => getLine >>=
      \input1 => putStr "Enter second string: " >>=
        \_ => getLine >>=
          \input2 =>
            putStrLn $ show $ largerInput input1 input2


printDo : IO ()
printDo = do
  putStr "Enter first string: "
  input1 <- getLine
  putStr "Enter second string: "
  input2 <- getLine
  let output = largerInput input1 input2
  putStrLn $ show output


main : IO ()
main = printBind
