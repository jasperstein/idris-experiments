module Ex1to3

import Data.Vect

Matrix: Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

testMatrix: Matrix 2 3
testMatrix = [ [0,0,0], [0,0,0]]


data Args : Type where
    Arg: Show a => Args 
    Lit: String -> Args

Structure: Type
Structure = List Args

toStructure: List Char -> Structure
toStructure [] = []
toStructure ('%' :: 's' :: rest) = Arg {a = String} :: toStructure rest
toStructure ('%' :: 'd' :: rest) = Arg {a = Double} :: toStructure rest
toStructure ('%' :: 'b' :: rest) = Arg {a = Bool} :: toStructure rest
toStructure ('%' :: 'c' :: rest) = Arg {a = Char} :: toStructure rest
-- toStructure other = Lit (pack (takeWhile (/= '%') other)) :: toStructure (dropWhile (/= '%') other)
toStructure (c :: rest) = Lit (cast c) :: toStructure rest

PrintfType: Structure -> Type
PrintfType [] = String
PrintfType (Arg {a} :: xs) = a -> PrintfType xs
PrintfType ((Lit x) :: xs) = PrintfType xs

printfAcc: (s: Structure) -> (acc: String) -> PrintfType s
printfAcc [] acc = acc
printfAcc ((Arg {a}) :: xs) acc = \x: a => printfAcc xs (acc ++ (show x))
printfAcc ((Lit x) :: xs) acc = printfAcc xs (acc ++ x)

printf: (fmt: String) -> PrintfType (toStructure (unpack fmt))
printf fmt = printfAcc (toStructure (unpack fmt)) ""

TupleVect: Nat -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test: TupleVect 4 Nat
test = (1, (2, (3, (4, ()))))
