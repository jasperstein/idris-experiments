module Ex1to3

import Data.Vect

transposeMat: Vect n (Vect k elem) -> Vect k (Vect n elem)
transposeMat {k} [] = replicate k []
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

addMat: Num elem => Vect n (Vect k elem) -> Vect n (Vect k elem) -> Vect n (Vect k elem)
addMat [] [] = []
addMat (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMat xs ys

inprod : Num elt => (x : Vect m elt) -> (ys : Vect k (Vect m elt)) -> Vect k elt
inprod x ys = map (\v => foldl (+) 0 (zipWith (*) x v)) ys

multMat: Num elt => Vect n (Vect m elt) -> Vect m (Vect k elt) -> Vect n (Vect k elt)
multMat [] ys = []
multMat (x :: xs) ys = inprod x (transposeMat ys) :: multMat xs ys

Main.main: IO ()
Main.main = do
    putStrLn (show (transposeMat [[1,2], [3,4], [5,6]]))
    putStrLn (show (addMat [[1,2], [3,4]] [[5,6], [7,8]]))
    putStrLn (show (multMat [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]))
