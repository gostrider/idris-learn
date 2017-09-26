module Exercise_4_2

data PowerSource = Petrol | Pedal

data Vehicle p = Bicycle p
               | Car Nat p
               | Bus Nat p
               | Unicycle p
               | Motorcycle Nat p

wheels : Vehicle power -> Nat
wheels (Bicycle x) = ?wheels_rhs_1
wheels (Car k x) = ?wheels_rhs_2
wheels (Bus k x) = ?wheels_rhs_3
wheels (Unicycle x) = ?wheels_rhs_4
wheels (Motorcycle k x) = ?wheels_rhs_5


refuel : Vehicle Petrol -> Vehicle Petrol
