module Exercise

palindrome : Nat -> String -> Bool
palindrome len x =
  if length x > len
    then
      let lower_x = toLower x
      in (==) lower_x (reverse lower_x)
    else
      False


counts : String -> (Nat, Nat)
counts str = ( length $ Strings.words str
             , length $ unpack str
             )


top_ten : Ord a => List a -> List a
top_ten xs = take 10 $ reverse xs


over_length : Nat -> List String -> Nat
over_length _ [] = 0
over_length k xs = length $ filter ((> k) . length) xs


main : IO ()
main = do
  putStr "Enter a string: "
  in_str <- getLine
  putStrLn $ show $ palindrome 0 in_str
  main
