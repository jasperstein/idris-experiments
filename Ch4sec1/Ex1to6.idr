module Ex1to6

data BSTree : Type -> Type where
    Empty : Ord elem => BSTree elem
    Node : Ord elem => (left : BSTree elem) -> (val : elem) ->
                            (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
        LT => Node (insert x left) val right
        EQ => orig
        GT => Node left val (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ val :: treeToList right

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = let max = if compare x y == LT then y else x in Just max


data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double


area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture


testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

biggestTriangle: Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle x y)) = Just (area t)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine x y) = case biggestTriangle x of
     Nothing => biggestTriangle y
     Just ax => case biggestTriangle y of
        Nothing => Just ax
        Just ay => Just (max ax ay)
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z
