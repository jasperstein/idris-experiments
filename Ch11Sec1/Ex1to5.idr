module Ex1to5

-- Ex 1
every_other: Stream a -> Stream a
every_other (a :: b :: rest) = b :: every_other rest

-- Ex 2

data InfList : Type -> Type where
    (::) : (value : elem) -> Inf (InfList elem) -> InfList elem
%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

implementation Functor InfList where
  map f (a :: rest) = (f a) :: map f rest

-- Ex 3

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

data Face = Heads | Tails

-- using this will not reduce getFace k inside the list
-- getFace: Int -> Face
-- getFace n = if n `mod` 2 == 0 then Heads else Tails

coinFlips: (count: Nat) -> Stream Int -> List Face
coinFlips i xs = let ints = take i xs in map (\n => if n `mod` 2 == 0 then Heads else Tails) ints

-- Ex 4
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: square_root_approx number next
    where
        next: Double
        next = (approx + (number / approx)) / 2

-- Ex 5

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound (value :: xs) = 
    if (abs(value * value - number) < bound)
        then value
        else square_root_bound k number bound xs
    
total 
square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                 (square_root_approx number number)