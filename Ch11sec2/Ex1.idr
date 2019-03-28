%default total

data InfIO : Type where
     Do : IO a
          -> (a -> Inf InfIO)
          -> InfIO

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = putStrLn "Out of fuel"

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = 
    Do {a = String} 
        (do putStr prompt
            input <- getLine
            let output = (action input)
            putStr output
            pure output)
        (\_ => totalREPL prompt action)
