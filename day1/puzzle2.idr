import puzzleOne
import routeParsing
import route

location : Position -> (Int, Int)
location (d, x, y) = (x, y)

walkPast : Distance -> Position -> List Position
walkPast n p = if n==0 then [] 
                       else let oneStep = walkStraight 1 p
                             in oneStep :: walkPast (n-1) oneStep

intermediatePositions : Position -> Move -> List Position
intermediatePositions p (t, i) = walkPast i $ faceNextDirection t p

isPositionAlreadyVisited : List (Int, Int) -> Position -> Bool
isPositionAlreadyVisited visited p = elem (location p) visited

findAlreadyVisited : List Position -> List (Int, Int) -> Maybe Position
findAlreadyVisited candidates visited = find (isPositionAlreadyVisited visited) candidates

Track : Type
Track = (Position, List (Int, Int))

nextMove : Move -> Track -> Either Position Track
nextMove m t = let intermediate = intermediatePositions (fst t) m in
			   let finalPos = last ((North, 0, 0) :: intermediate) in
				 case (findAlreadyVisited intermediate (snd t)) of
					Just pos => Left pos
					Nothing  => Right (finalPos, ((map location intermediate) ++ (snd t)))

processNextMove : Either Position Track -> Move -> Either Position Track
processNextMove e m = case e of
						Left p => Left p
						Right t => nextMove m t

processMoves : List Move -> Either Position Track
processMoves moves = foldl processNextMove (Right ((North, 0, 0), [])) moves

example : Either Position Track
example = processMoves (parsedMoves "R8, R4, R4, R8")

distance : Either Position Track -> Maybe Int
distance (Left (d, x, y)) = Just ((abs x) + (abs y))
distance (Right t) = Nothing


puzzle2Solution : Maybe Int
puzzle2Solution = distance (processMoves puzzle1Moves)
