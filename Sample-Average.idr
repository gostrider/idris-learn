module Average

||| Calculate the average length of words in a string.
||| @str a string containing words separated by whitespace.
export
average : (str : String) -> Double
average str =
  let
    numWords = wordCount str
    totalLength = sum (allLengths (words str))
  in
    cast totalLength / cast numWords
  where
    wordCount : String -> Nat
    wordCount = length . words

    allLengths : List String -> List Nat
    allLengths strs = map length strs

showAverage : String -> String
showAverage str = "Average word length is: " ++ show (average str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showAverage