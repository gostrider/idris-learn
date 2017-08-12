module WordLength

allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

printNext : Nat -> List Nat -> List Nat
printNext Z acc = acc
printNext (S k) acc = printNext k (k :: acc)
