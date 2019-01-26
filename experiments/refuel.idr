data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel Bus = Bus 200