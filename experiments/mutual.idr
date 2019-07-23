import System
import Data.Primitives.Views

x: Int
y: Int
z: Int

x = 1
y = 2
z = 3


data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do


correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat

correct nums score
        = do PutStr "Correct!\n"
             quiz nums (score + 1)

wrong nums ans score
    = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
         quiz nums score

quiz (num1 :: num2 :: nums) score
    = do PutStr ("Score so far: " ++ show score ++ "\n")
         PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
         answer <- GetLine
         if toLower answer == "quit" then Quit score else
         if (cast answer == num1 * num2)
            then correct nums score
            else wrong nums (num1 * num2) score

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x 
runCommand GetLine = getLine

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
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
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
              | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)