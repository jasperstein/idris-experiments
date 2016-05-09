import range

Position : Type
Position = (Integer, Integer)

board : List Position
board = range2 (1,1) (8,8)

using (x: Integer, x1: Integer, x2: Integer, y: Integer, y1: Integer, y2: Integer)
	data Attacks : Position -> Position -> Type where
		Hor_attack: Attacks (x1, y) (x2, y)
		Vert_attack: Attacks (x, y1) (x, y2)
		Diag_attack: abs(x1 - x2) = abs(y1 - y2) -> Attacks (x1, y1) (x2, y2)

dec_attack : (p1: Position) -> (p2: Position) -> Dec (Attacks p1 p2)
dec_attack (x1, y1) (x2, y2) with (decEq y1 y2)
  dec_attack (x1, y)  (x2, y)  | Yes Refl = Yes Hor_attack
  dec_attack (x1, y1) (x2, y2) | No ysNeq  with (decEq x1 x2)
  	dec_attack (x, y1)  (x, y2)  | No ysNeq | Yes Refl = Yes Vert_attack
  	dec_attack (x1, y1) (x2, y2) | No ysNeq | No xsNeq with (decEq (abs (x1-x2)) (abs (y1-y2)))
  		dec_attack (x1, y1) (x2, y2)  | No ysNeq | No xsNeq | Yes h = Yes (Diag_attack h)
  		dec_attack (x1, y1) (x2, y2)  | No ysNeq | No xsNeq | No diagNeq = No (\hA => case hA of
  				Hor_attack => ysNeq Refl
  				Vert_attack => xsNeq Refl
  				Diag_attack hDiag => diagNeq hDiag)


main : IO ()
main = putStrLn $ show board