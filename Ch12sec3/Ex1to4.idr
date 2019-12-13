import System
import Data.Primitives.Views

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

record Score where
    constructor MkScore
    correct : Nat
    attempted : Nat

record GameState where
    constructor MkGameState
    score : Score
    difficulty : Int

initState : GameState
initState = MkGameState (MkScore 0 0) 12

data Command : Type -> Type where 
    PutStr : String -> Command () 
    GetLine : Command String
    GetRandom : Command Int
    GetGameState : Command GameState 
    PutGameState : GameState -> Command ()
    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b

mutual
    Functor Command where
        map f cx = do
            x <- cx
            pure (f x)

    Applicative Command where
        pure = Pure
        (<*>) cf cy = do
            f <- cf
            y <- cy
            Pure (f y)

    Monad Command where
        (>>=) = Bind
        -- join ccx = do
        --     cx <- ccx
        --     cx

-- namespace CommandDo
--     (>>=) : Command a -> (a -> Command b) -> Command b
--     (>>=) = Bind

data ConsoleIO : Type -> Type where 
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    
namespace ConsoleDo
    (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    (>>=) = Do
    
updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do
    s <- GetGameState
    PutGameState (f s)

Show GameState where
    show st = show (correct (score st)) ++ "/" ++
              show (attempted (score st)) ++ "\n" ++
              "Difficulty: " ++ show (difficulty st)

addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }

addCorrect : GameState -> GameState 
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1) }

data Input: Type where
    Answer: Int -> Input
    QuitCmd: Input

mutual
    readInput : (prompt : String) -> Command Input
    readInput prompt = do 
        PutStr prompt
        answer <- GetLine
        if toLower answer == "quit"
            then Pure QuitCmd
            else Pure (Answer (cast answer))

    quiz : ConsoleIO GameState
    quiz = do
        num1 <- GetRandom
        num2 <- GetRandom
        st <- GetGameState 
        PutStr (show st ++ "\n")
        input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ") 
        case input of
            Answer answer => if answer == num1 * num2
                                then correct
                                else wrong (num1 * num2)
            QuitCmd => Quit st

    correct : ConsoleIO GameState
    correct = do 
        PutStr "Correct!\n"
        updateGameState addCorrect
        quiz

    wrong : Int -> ConsoleIO GameState
    wrong actual = do 
        PutStr ("Wrong! Should be" ++ (show actual) ++ "\n")
        updateGameState addWrong
        quiz
    
randoms : Int -> Stream Int 
randoms seed = let seed' = 1664525 * seed + 1013904223 in
        (seed' `shiftR` 2) :: randoms seed'

runCommand : Stream Int -> GameState -> Command a -> IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do 
    putStr x
    pure ((), rnds, state)

runCommand rnds state GetLine = do 
    str <- getLine
    pure (str, rnds, state)

runCommand (val :: rnds) state GetRandom
    = pure (getRandom val (difficulty state), rnds, state)
        where
            getRandom : Int -> Int -> Int
            getRandom val max with (divides val max)
                getRandom val 0 | DivByZero = 1
                getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1

runCommand rnds state GetGameState
        = pure (state, rnds, state)

runCommand rnds state (PutGameState newState)
        = pure ((), rnds, newState)

runCommand rnds state (Pure val)
        = pure (val, rnds, state)

runCommand rnds state (Bind c f) = do
    (res, newRnds, newState) <- runCommand rnds state c
    runCommand newRnds newState (f res)

run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> IO (Maybe a, Stream Int, GameState)
run fuel rnds state (Quit val) = do
    pure (Just val, rnds, state)
run (More fuel) rnds state (Do c f) = do 
    (res, newRnds, newState) <- runCommand rnds state c 
    run fuel newRnds newState (f res)
run Dry rnds state p = pure (Nothing, rnds, state)

partial
main : IO ()
main = do 
    seed <- time
    (Just score, _, state) <- run forever (randoms (fromInteger seed)) initState quiz
        | _ => putStrLn "Ran out of fuel"
    putStrLn ("Final score: " ++ show state)
        


record Votes where
        constructor MkVotes
        upvotes : Integer
        downvotes : Integer

record Article where
        constructor MkArticle
        title : String
        url : String
        score : Votes

initPage : (title : String) -> (url : String) -> Article 
initPage title url = MkArticle title url (MkVotes 0 0)

getScore : Article -> Integer
getScore article = upvotes (score article) - downvotes (score article)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

addUpvote : Article -> Article 
addUpvote = record { score->upvotes $= (+ 1)}

addDownvote : Article -> Article
addDownvote = record { score->downvotes $= (+ 1)}
