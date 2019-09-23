import Control.Monad.State

update: (stateType -> stateType) -> State stateType ()
update f = 
    do x <- get
       put (f x) 

increase: Nat -> State Nat ()
increase x = update (+x)


data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

countEmpty: Tree a -> State Nat ()
countEmpty Empty = increase 1
countEmpty (Node l item r) =
    do countEmpty l
       countEmpty r

countEmptyNode: Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = 
    do (e, n) <- get
       put (e + 1, n)
countEmptyNode (Node l item r) = 
    do countEmptyNode l
       countEmptyNode r
       (e, n) <- get
       put (e, n + 1)