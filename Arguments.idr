module Arguments

import Data.Vect

length : Vect n elem -> Nat
length {n} _ = n

createEmpties : Vect n (Vect 0 a)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties
