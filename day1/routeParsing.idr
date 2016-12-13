module RouteParsing

import route
import Data.String

%access public export

moves : String -> List String
moves directions = map trim $ split (== ',') directions

turnFromChar : Char -> Turn
turnFromChar 'L' = GoLeft
turnFromChar 'R' = GoRight

parseMove : String -> Move
parseMove m = (turnFromChar (strHead m), fromMaybe 0 (parseInteger (strTail m)))

parsedMoves : String -> List Move
parsedMoves directions = map parseMove $ moves directions
