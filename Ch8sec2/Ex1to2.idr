module Ex1to2

import Data.Vect

mypluscomm : (n: Nat) -> (k: Nat) -> n+k = k+n
mypluscomm Z k = sym (plusZeroRightNeutral _)
mypluscomm (S j) k =
    let x = (plusSuccRightSucc k j)
        y = cong {f = S} (mypluscomm j k) in
    rewrite (sym x) in rewrite y in Refl

prf1 : (acc : Vect n1 a) -> Vect (plus n1 0) a
prf1 {n1} acc = let x = plusZeroRightNeutral n1 in rewrite x in acc

p : (n : Nat) -> (k : Nat) -> plus n (S k) = S (plus n k)
p n k = let x = plusSuccRightSucc n k in rewrite x in Refl

prf2 : Vect ((S n1) + len) a -> Vect (plus n1 (S len)) a
prf2 {n1} {len} xs = let x: (plus n1 (S len) = S (plus n1 len)) = p n1 len in 
    rewrite x in xs

myrev: Vect n a -> Vect n a
myrev xs = aux [] xs
  where
    aux: Vect n a -> Vect m a -> Vect (n+m) a
    aux acc [] = prf1 acc
    aux acc (y::ys) = prf2 (aux (y::acc) ys) 
