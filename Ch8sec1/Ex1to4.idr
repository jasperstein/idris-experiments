module Ex1to4

same_cons: {xs: List a} -> {ys: List a} -> xs = ys -> x :: xs = x :: ys
same_cons {x} prf = cong {f = (x ::)} prf

same_lists: {xs: List a} -> {ys: List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists prf prf1 = case prf of Refl => same_cons prf1

data ThreeEq: a -> b -> c -> Type where
    Threfl: (x: a) -> ThreeEq x x x

allSameS: (x, y, z: Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (Threfl z) = Threfl (S z)
