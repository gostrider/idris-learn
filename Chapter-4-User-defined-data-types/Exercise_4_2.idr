module Exercise_4_2

import Data.Vect

data PowerSource = Petrol | Pedal | Electric

-- Exercise 4.2.1
data Vehicle : PowerSource -> Type where
  Unicycle    :                 Vehicle Pedal
  Bicycle     :                 Vehicle Pedal
  Motorcycle  : (fuel : Nat) -> Vehicle Petrol
  Car         : (fuel : Nat) -> Vehicle Petrol
  Bus         : (fuel : Nat) -> Vehicle Petrol
  Tram        :                 Vehicle Electric


-- Exercise 4.2.2
wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels (Car _) = 4
wheels (Bus _) = 4
wheels Tram = 8


-- Exercise 4.2.2
refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200


{-
Exercise 4.2.4
Different with:

vectTake : (n : Nat) -> Vect m elem -> Vect n

As there is no relation between n & m
For example,
m = 5, where n do not included as part of the length

Suppose
TypeError when n > m
n is bounded by m

-}
vectTake : (n : Nat) -> Vect (n + m) elem -> Vect n elem
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs


-- Exercise 4.2.5
sumEntries : Num elem => (pos : Integer) -> Vect n elem -> Vect n elem -> Maybe elem
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just x) => Just $ (index x xs) + (index x ys)
