module Main


main : IO ()
main = putStrLn "runnable"

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int


getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt True = 0
getStringOrInt False = "runnable"


valToString : (x : Bool) -> StringOrInt x -> String
valToString False y = y
valToString True y = cast y
