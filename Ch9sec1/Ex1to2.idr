module Ex1to2

data Elem: a -> List a -> Type where
    Here: Elem x (x::xs)
    There: {auto pf: Elem x xs} -> Elem x (y::xs)


data Last : List a -> a -> Type where
    LastOne : Last [value] value
    LastCons : (prf : Last xs value) -> Last (x :: xs) value


isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No (\l => case l of LastOne impossible)
isLast (x :: []) value = case decEq x value of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (\l => contra (case l of
                                 LastOne => Refl
                                 (LastCons prf) impossible))
isLast (x :: (y::rest)) value = case isLast (y::rest) value of
                                (Yes prf) => Yes (LastCons prf)
                                (No cont) => No (\l => case l of 
                                    (LastCons prf) => cont prf)
