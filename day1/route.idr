module Route

%access public export

data Direction = North | East | South | West

data Turn = GoLeft | GoRight

Distance : Type
Distance = Int

Move : Type
Move = (Turn, Distance)

Position : Type
Position = (Direction, Int, Int)

walk : Distance -> Direction -> (Int, Int) -> (Int, Int)
walk d North (x, y) = (x, y+d)
walk d East (x, y) = (x+d, y)
walk d South (x, y) = (x, y-d)
walk d West (x, y) = (x-d, y)

walkStraight : Distance -> Position -> Position
walkStraight i (dir, x, y) = let (x', y') = walk i dir (x, y) in (dir, x', y')

nextDirection : Turn -> Direction -> Direction
nextDirection GoRight North = East
nextDirection GoRight East = South
nextDirection GoRight South = West
nextDirection GoRight West = North
nextDirection GoLeft North = West
nextDirection GoLeft East = North
nextDirection GoLeft South = East
nextDirection GoLeft West = South

faceNextDirection : Turn -> Position -> Position
faceNextDirection t (p, x, y) = (nextDirection t p, x, y)

executeTurn : Position -> Move -> Position
executeTurn p (t, i) = walkStraight i $ faceNextDirection t p

