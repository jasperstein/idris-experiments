module Ex1to5

import Data.Fin
import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    UniCycle : Vehicle Pedal
    MotorCycle: (fuel: Nat) -> Vehicle Petrol
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol
    Tram : Vehicle Electric
    ECar: Vehicle Electric

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
refuel UniCycle impossible
refuel (MotorCycle fuel) = MotorCycle 30



vectTake: (k: Fin n) -> Vect n elem -> Vect (finToNat k) elem
vectTake FZ v = []
vectTake (FS fn) (x :: xs) = x :: (vectTake fn xs)

vectTake2: (k:Nat) -> Vect (k+n) elem -> Vect k elem
vectTake2 Z v = []
vectTake2 (S fn) (x :: xs) = x :: (vectTake2 fn xs)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                            Nothing => Nothing
                            (Just x) => Just ((index x xs) + (index x ys))

