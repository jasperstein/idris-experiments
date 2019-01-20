module Ex1to4

import Data.Vect

my_length: List a -> Nat
my_length [] = 0
my_length (x :: xs) = S (my_length xs)

my_reverse: List a -> List a
my_reverse xs = my_rev_aux xs (the (List a) [])
    where my_rev_aux [] aux = aux
          my_rev_aux (x :: xs) aux = my_rev_aux xs (x::aux)

    
my_map: (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vect_map: (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
