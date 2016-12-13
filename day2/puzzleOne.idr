data NBRS = Neighbours Nat Nat Nat Nat

Number : Type
Number = Nat

neighbours : List NBRS
neighbours = [
	Neighbours 0 0 0 0,
	Neighbours 1 2 4 1,
	Neighbours 2 3 5 1,
	Neighbours 3 3 6 2,
	Neighbours 1 5 7 4,
	Neighbours 2 6 8 4,
	Neighbours 3 6 9 5,
	Neighbours 4 8 7 7,
	Neighbours 5 9 8 7,
	Neighbours 6 9 9 8
]

neighbours2 : List NBRS
neighbours2 = [
	Neighbours 0 0 0 0,
	Neighbours 1 1 3 1,
	Neighbours 2 3 6 2,
	Neighbours 1 4 7 2,
	Neighbours 4 4 8 3,
	Neighbours 5 6 5 5,
	Neighbours 2 7 10 5,
	Neighbours 3 8 11 6,
	Neighbours 4 9 12 7,
	Neighbours 9 9 9 8,
	Neighbours 6 11 10 10,
	Neighbours 7 12 13 10,
	Neighbours 8 12 12 11,
	Neighbours 11 13 13 13
]

up : List NBRS -> (i:Nat) -> Nat
up neighbours i = case index' i neighbours of Just (Neighbours u _ _ _) => u

right : List NBRS -> (i:Nat) -> Nat
right neighbours i = case index' i neighbours of Just (Neighbours _ e _ _) => e

down : List NBRS -> (i:Nat) -> Nat
down neighbours i = case index' i neighbours of Just (Neighbours _ _ s _) => s

left : List NBRS -> (i:Nat) -> Nat
left neighbours i = case index' i neighbours of Just (Neighbours _ _ _ w) => w


--parseInput : String -> List (List Char)
--parseInput s = map unpack $ lines s

nextDigit :  List NBRS -> (i:Nat) -> List Char -> Nat
nextDigit neighbours i [] = i
nextDigit neighbours i (c :: cs) = case c of
	'U' => nextDigit neighbours (up neighbours i) cs
	'R' => nextDigit neighbours (right neighbours i) cs
	'D' => nextDigit neighbours (down neighbours i) cs
	'L' => nextDigit neighbours (left neighbours i) cs

prependNextDigit : List NBRS ->  List Nat -> List Char -> List Nat
prependNextDigit neighbours [] cs = [nextDigit neighbours 5 cs]
prependNextDigit neighbours nns@(n::ns) cs = let next = nextDigit neighbours n cs in next :: nns

reversedCode : List NBRS -> List (List Char) -> List Nat
reversedCode neighbours css = foldl (prependNextDigit neighbours) [] css

getCode : List NBRS -> List String -> List Nat
getCode neighbours s = reverse . (reversedCode neighbours) $ map unpack s

example : List String
example = ["ULL",
"RRDDD",
"LURDL",
"UUUUD"]

puzzle1Input : List String
puzzle1Input = ["LRULLRLDUUUDUDDDRLUDRDLDDLUUDLDDLRDRLDRLLURRULURLDRLDUDURLURRULLDDDUDDRRRDLRRDDLDURDULLRDLLLDRDLLDULDUDLLDLDRUDLLDLDDRRRDRLUDRDDLUDRRDUDUDLLDDUUDLRDUDRRUDUDRULRULUDRUUDLDLULLRLDLDDRULLRLLLULUULDURURLUUULDURLDDDURRUUDURDDDULDLURLRDRURDRUDRLLDLDRUURLLLRDRURUDLRLUDULLDDURLRURDLRDUUURRLULRRLDDULUUURLRRRLLLLLURDDRUULUDRRRUDDLLULRRUULDRDDULRLDDDRRUULUDRLRUDURUUULDLDULUUDURLLLRRDDRDLURDDDLDDDLRDRLDDURLRLLRUDRRLLDDDDDURDURRDDULDULLRULDRUURDRRDUDDUDDDDRRDULDUURDRUDRLDULRULURLLRRDRDRDLUUDRRLRLDULDDLUUUUUURRLRRRULLDDDRLRDRRRRRRRDUUDLLUDURUDDLURRUDL",
"UDUUURRLRLLDDRRDRRRLDDDLURURLLUDDRLUUDRRRDURRLLRURDLLRRDUUDDDDRDRURRLLLLURDLRRRULLLDLLLUDDLDRRRDLDUUDDRDUDDUURDDLULULDURDURDRUULURURRURDUURUDRRUDRLLLLRRDLLDRDDRLLURDDDUDUDUDRUURDDRUURDLRUUDDRDUURUDDLLUURDLUDRUUDRRDLLUUURDULUULDUUDLLULUUDLUDRUUDUUURLDDDRLRURDDULLRDRULULUDLUUDDDUUDLDUUDRULLDUURDDRUDURULDRDDLRUULRRRDLDLRDULRDDRLLRRLURDLDRUDLRLUDLRLDLDURRUULRLUURDULDRRULLRULRDLLDLDUDRUDDUDLDDURDDDRDLUDRULRUULLRURLDDDRDLRRDRULURULDULRDLDULDURDRDRDRDURDRLUURLRDDLDDRLDDRURLLLURURDULDUDDLLUURDUUUDRUDDRDLDRLRLDURRULDULUUDDLRULDLRRRRDLLDRUUDRLLDLUDUULRDRDLRUUDLRRDDLUULDUULRUDRURLDDDURLRRULURR",
"LDURLLLRLLLUURLLULDLRLLDLURULRULRDUDLDDUDRLRRDLULLDDULUUULDRLDURURLURLDLRUDULLLULDUURLLRDLUULRULLLULRDRULUDLUUULDDURLUDDUDDRDLDRDRUDLUURDDLULDUULURLUULRDRDLURUDRUDLDRLUUUUULUDUDRRURUDRULDLDRDRLRURUUDRDLULLUDLLRUUDUUDUDLLRRRLDUDDDRDUDLDLLULRDURULLLUDLLRUDDUUDRLDUULLDLUUDUULURURLLULDUULLDLUDUURLURDLUULRRLLRUDRDLLLRRRLDDLUULUURLLDRDLUUULLDUDLLLLURDULLRUDUUULLDLRLDRLLULDUDUDRULLRRLULURUURLRLURRLRRRDDRLUDULURUDRRDLUDDRRDRUDRUDLDDRLRDRRLDDRLLDDDULDLRLDURRRRRULRULLUUULUUUDRRDRDRLLURRRRUULUDDUDDDLDURDRLDLLLLLRDUDLRDRUULU",
"URURRUUULLLLUURDULULLDLLULRUURRDRRLUULRDDRUDRRDUURDUDRUDDRUULURULDRLDRDDDLDLRLUDDRURULRLRLLLDLRRUDLLLLRLULDLUUDUUDRDLRRULLRDRLRLUUDDRRLLDDRULLLRLLURDLRRRRRLLDDRRDLDULDULLDLULLURURRLULRLRLLLLURDDRDDDUUDRRRDUUDDLRDLDRRLLRURUDUUUDLDUULLLRLURULRULRDRLLLDLDLRDRDLLLRUURDDUDDLULRULDLRULUURLLLRRLLLLLLRUURRLULRUUUDLDUDLLRRDDRUUUURRRDRRDULRDUUDULRRRDUUUUURRDUURRRRLDUDDRURULDDURDDRDLLLRDDURUDLLRURLRRRUDDLULULDUULURLUULRDLRDUDDRUULLLRURLDLRRLUDLULDRLUDDDRURUULLDLRLLLDULUDDRLRULURLRDRRDDLDLURUDDUUURRDDLUDDRDUULRRDLDRLLLULLRULRURULRLULULRDUD",
"RUDLLUDRRDRRLRURRULRLRDUDLRRLRDDUDRDLRRLLRURRDDLRLLRRURULRUULDUDUULDULDLRLRDLRDLRUURLDRLUDRRDDDRDRRRDDLLLRRLULLRRDDUDULRDRDUURLDLRULULUDLLDRUDUURRUDLLRDRLRRUUUDLDUDRRULLDURRDUDDLRURDLDRLULDDURRLULLRDDDRLURLULDLRUDLURDURRUDULDUUDLLLDDDUUURRRDLLDURRDLULRULULLRDURULLURDRLLRUUDDRRUDRDRRRURUUDLDDRLDRURULDDLLULULURDLDLDULLRLRDLLUUDDUDUDDDDRURLUDUDDDRRUDDLUDULLRDLDLURDDUURDLRLUUDRRULLRDLDDDLDULDUDRDUUULULDULUDLULRLRUULLDURLDULDRDLLDULLLULRLRD"]

