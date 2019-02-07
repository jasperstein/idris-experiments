module Ex1to2

import Ch7sec2.Ex1to3

export data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs
            
Functor Expr where
  map f (Val a) = Val (f a)
  map f (Add a b) = Add (map f a) (map f b)
  map f (Sub a b) = Sub (map f a) (map f b)
  map f (Mul a b) = Mul (map f a) (map f b)
  map f (Div a b) = Div (map f a) (map f b)
  map f (Abs a) = Abs (map f a)



data Vect : Nat -> Type -> Type where
    Nil : Vect 0 elem
    (::) : elem -> Vect len elem -> Vect (S len) elem

Eq a => Eq (Vect n a) where
  (==) [] [] = True
  (==) (x :: z) (y :: w) = x==y && z==w
--   (/=) x y = ?Eq_rhs

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: y) = func x (foldr func init y)
--   foldl func init input = ?Foldable_rhs_2
