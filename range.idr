module Range

%access public export

range2 : (Enum a, Enum b) => (a,b) -> (a,b) -> List (a,b)
range2 (a1, b1) (a2, b2) = join (map (\a => map (\b => (a,b)) (enumFromTo b1 b2)) (enumFromTo a1 a2))

