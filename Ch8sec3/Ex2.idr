module Ex2

data Vect: Nat -> Type -> Type where
    VNil: Vect 0 a
    VSuc: a -> Vect k a -> Vect (S k) a

p_3 : (prf : x = z) -> (contra : (y = w) -> Void) -> (VSuc x y = VSuc z w) -> Void
p_3 prf contra Refl = contra Refl

p_2 : (contra : (x = z) -> Void) -> (VSuc x y = VSuc z w) -> Void
p_2 contra Refl = contra Refl

p_1 : (prf : x = z) -> (prf1 : y = w) -> VSuc x y = VSuc z w
p_1 Refl Refl = Refl

DecEq a => DecEq (Vect n a) where
  decEq VNil VNil = Yes Refl
  decEq (VSuc x y) (VSuc z w) = case (decEq x z) of 
    (Yes prf) => case (decEq y w) of (Yes prf1) => Yes (p_1 prf prf1)
    (Yes prf) => case (decEq y w) of (No contra) => No (p_3 prf contra)
    (No contra) => No (p_2 contra)
