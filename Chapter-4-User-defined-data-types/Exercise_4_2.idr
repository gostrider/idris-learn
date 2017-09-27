module Exercise_4_2

data PowerSource = Petrol | Pedal | Electric


data Vehicle : PowerSource -> Type where
  Unicycle    :                 Vehicle Pedal
  Bicycle     :                 Vehicle Pedal
  Motorcycle  : (fuel : Nat) -> Vehicle Petrol
  Car         : (fuel : Nat) -> Vehicle Petrol
  Bus         : (fuel : Nat) -> Vehicle Petrol



wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels (Car _) = 4
wheels (Bus _) = 4


refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
