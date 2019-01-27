module Ex1to4

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

guess : (target: Nat) -> IO ()
guess target = 
    do  printLn "Guess the number: "
        Just i <- readNumber | Nothing => do 
            printLn "Please enter a number"
            guess target
        case compare i target of
            GT => do printLn "Your guess is too high!"
                     guess target
            LT => do printLn "Your guess is too low!"
                     guess target
            EQ => do printLn "You guessed it!"
       
guess2 : (target: Nat) -> (attemptNr: Nat) -> IO ()
guess2 target attemptNr = 
    do  printLn ("Attempt nr " ++ show attemptNr)
        printLn "Guess the number: "
        Just i <- readNumber | Nothing => do 
            printLn "Please enter a number"
            guess2 target attemptNr
        case compare i target of
            GT => do printLn "Your guess is too high!"
                     guess2 target (attemptNr + 1)
            LT => do printLn "Your guess is too low!"
                     guess2 target (attemptNr + 1)
            EQ => do printLn "You guessed it!"
                     printLn ("It took you only " ++ (show attemptNr) ++ " attempts.")
       
main : IO ()
main = do t <- System.time
          let target = mod t 100
          guess2 (cast target) 1

myRepl: String -> (String -> String) -> IO ()
myRepl prompt onInput = do
    putStr prompt
    input <- getLine
    putStr (onInput input)
    myRepl prompt onInput

myReplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do
    putStr prompt
    input <- getLine
    case (onInput state input) of
        Nothing => pure ()
        Just (reply, newState) => do
            putStr reply
            myReplWith newState prompt onInput
