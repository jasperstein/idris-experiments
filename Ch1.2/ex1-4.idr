import Data.Vect
import Data.Fin

aux: (n: Nat) -> n*2 = n+n
aux Z = Refl
aux (S k) = let x = plusSuccRightSucc (S k) k
                y = succInjective _ _ x
                z = aux k in rewrite z in rewrite y in Refl

ex1: Vect n elem -> Vect n elem
ex1 = id

ex2: Vect n elem -> Vect (n*2) elem
ex2 {n} v = let x: ((n*2) = (n+n)) = aux n in rewrite x in v ++ v

ex3: Vect (1+n) elem -> Vect n elem
ex3 = tail

ex4: Fin n -> Vect n elem -> elem
ex4 = index