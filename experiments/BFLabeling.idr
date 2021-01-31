import Data.Vect

data Tree: (a: Type) -> Type where
    Node: a -> List (Tree a) -> Tree a

implementation Show a => Show (Tree a) where
    show (Node a ts) = "{" ++ show a ++ show ts ++ "}"

children: Tree a -> List (Tree a)
children (Node x xs) = xs


testTree: Tree String
testTree = Node "root" [
    Node "left" [
        Node "ll" [],
        Node "lm" [
            Node "lml" [],
            Node "lmr" []
        ],
        Node "lr" []
    ],
    Node "right" [
        Node "rm" []
    ]
]

testTree2: Tree String
testTree2 = Node "root" [
    Node "left" [
        Node "ll" [],
        Node "lm" [
            Node "lml" [],
            Node "lmr" []
        ],
        Node "lr" []
    ],
    Node "mid" [
        Node "ml" [],
        Node "mr" [
            Node "mrm" [
                Node "mrmm" []
            ]
        ]
    ],
    Node "right" [
        Node "rm" []
    ]
]

nodeWidths: List (Tree a) -> List Nat
nodeWidths xs = 
    let allChildren = concat $ map children xs
    in if (length allChildren == 0) then [length xs] else ((length xs) :: (nodeWidths allChildren))

widths: Tree a -> List Nat
widths (Node s ts) = 1 :: nodeWidths ts

size: Tree a -> Nat
size t = sum $ widths t

testWidths: IO ()
testWidths = 
    do
        putStrLn . show $ widths testTree
        putStrLn . show $ the (List Int) [1,2,4,2]
        putStrLn . show $ size testTree
        putStrLn . show $ widths testTree2
        putStrLn . show $ the (List Int) [1,3,6,3,1]
        putStrLn . show $ size testTree2
    

(++): List a -> Stream a -> Stream a
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)


initial: Tree a -> Stream b -> (List (List Nat, List b), Stream b)
initial tree bs = (zip (map widths (children tree)) (replicate (size tree) []), bs)

step: (List (List Nat, List b), Stream b) -> (List (List Nat, List b), Stream b)
step ([], bs) = ([], bs)
step (([], labels)::xs, bs) = (xs ++ [([], labels)], bs)
step ((width::widths, labels)::xs, bs) = (xs ++ [(widths, labels ++ take width bs)], drop width bs)

max: List Nat -> Nat
max = foldl max 0

requiredSteps: List (List a) -> Nat
requiredSteps allWidths = length allWidths * max (map length allWidths)

childLabels: Tree a -> Stream b -> List (List b)
childLabels tree labels = 
    let init: (List (List Nat, List b), Stream b) = initial tree labels
        stepCount: Nat = requiredSteps $ map fst $ fst init
        finalStep: (List (List Nat, List b), Stream b) = head $ drop stepCount $ iterate step init
        answer: List (List b) = map snd $ fst finalStep
    in answer


labelTree: Tree a -> Stream b -> Tree (a,b)
labelTree tree@(Node nodeVal children) stream@(label :: labels) = 
    let chLabels = childLabels tree labels
        chLabelStreams = map (++ stream) chLabels
    in Node (nodeVal, label) (zipWith labelTree children chLabelStreams)
    


main: IO ()
main = 
    do  printLn $ labelTree testTree [1..]
        printLn $ labelTree testTree2 [1..]