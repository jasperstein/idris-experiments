data Fin: Nat -> Type where
    mkFin: (k: Nat) -> (pf: LT k n) -> Fin n

interface EQL a {
    | mv: a -> b
    | eql: a -> a -> Bool
    | irr: eql {a} x y -> irr x == irr y
}

instance EQL (Fin n) where
    mv (mkFin k _) = k
    eql (mkFin k1 _) (mkFin k2 _) = k1==k2
    irr (mkFin k1 _) (mkFin k2 _) = Refl
