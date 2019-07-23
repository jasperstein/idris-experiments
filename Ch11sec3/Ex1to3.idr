module Ex1to3

import System
import Data.Primitives.Views

data Input: Type where
    QuitCmd: Input
    Answer: Int -> Input
    Cat: String -> Input
    Copy: String -> String -> Input

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String
    WriteFile: String -> String -> Command ()
    ReadFile: String -> Command String
    Pure: a -> Command a
    Bind: Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
    Quit : a -> b -> ConsoleIO (a, b)
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
    (>>=) : Command a -> (a -> (Command b)) -> Command b
    (>>=) = Bind

namespace ConsoleDo
    (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    (>>=) = Do

readInput: String -> Command Input
readInput s = do PutStr s
                 a <- GetLine
                 let w = words a
                 case w of
                    w1 :: Nil => if (toLower w1 == "quit") then Pure QuitCmd else Pure (Answer (cast w1))
                    w1 :: w2 :: Nil => 
                        if (toLower w1 /= "cat") 
                        then Pure (Answer (cast a))
                        else Pure (Cat w2)
                    w1 :: w2 :: w3 :: Nil => 
                        if (toLower w1 /= "copy") 
                        then Pure (Answer (cast a))
                        else Pure (Copy w2 w3)
                    _ => Pure (Answer (cast a))
                    
correct: Stream Int -> Nat -> Nat -> ConsoleIO (Nat, Nat)
wrong: Stream Int -> (correctAnswer: Int) -> (score: Nat) -> Nat ->  ConsoleIO (Nat, Nat)
quiz : Stream Int -> (score : Nat) -> Nat -> ConsoleIO (Nat, Nat)
cat: Stream Int -> (score: Nat) -> (all: Nat) -> (filename: String) -> ConsoleIO (Nat, Nat)
copy: Stream Int -> (score: Nat) -> (all: Nat) -> (source: String) -> (dest: String) -> ConsoleIO (Nat, Nat)

quiz (num1 :: num2 :: nums) score all
    = do PutStr ("Score so far: " ++ show score ++ " / " ++ show all ++ "\n")
         input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
         case input of
              Answer answer => 
                  if answer == num1 * num2
                      then correct nums score all
                      else wrong nums (num1 * num2) score all
              QuitCmd => Quit score all
              Cat f => cat nums score all f
              Copy src dst => copy nums score all src dst


wrong nums correctAnswer score all
    = do PutStr ("Incorrect :-( The correct answer is: " ++ (show correctAnswer))
         quiz nums score (all + 1)

correct nums score all
    = do PutStr "Congratulations! That is correct!"
         quiz nums (score + 1) (all + 1)

cat nums score all file
    = do content <- ReadFile file
         PutStr content
         quiz nums score all

copy nums score all src dest
    = do content <- ReadFile src
         WriteFile dest content
         quiz nums score all

runCommand: Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind x f) = do a <- runCommand x 
                           runCommand (f a)
runCommand (WriteFile f cont) = do (Right e) <- writeFile f cont | Left e => putStrLn ("Could not write file " ++ f)
                                   pure ()
runCommand (ReadFile f) = do (Right e) <- readFile f | Left e => do { putStrLn ("Could not read file " ++ f) ; pure "" }
                             pure e

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit score all) = do pure (Just (score, all))
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'
                   
arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
        bound ((12 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do seed <- time
          Just (score, all) <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
              | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score ++ " / " ++ show all)

Main.main: IO ()
Main.main = Ex1to3.main