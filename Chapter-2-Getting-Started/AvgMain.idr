module AvgMain

import Average


showAverage : String -> String
showAverage str = "Average word length is: " ++ show (average str) ++ "\n"


main : IO ()
main = repl "Enter a string: " showAverage
