module Ex1to3

import Data.Vect

readToBlank: IO (List String)
readToBlank = do
    line <- getLine
    if line == ""
        then pure []
        else do lines <- readToBlank
                pure (line :: lines)

readAndSave: IO ()
readAndSave = do
    lines <- readToBlank
    filename <- getLine
    let contents = concat (map (\line => line ++ "\n") lines)
    Right res <- writeFile filename contents
        | Left => putStrLn ("Could not write file " ++ filename)
    pure () 

readVectFile: (filename: String) -> IO (n ** Vect n String)
readVectFile filename = do
    Right file <- openFile filename Read
        | Left => pure (_ ** [])
    result <- readLines file (_ ** [])
    closeFile file
    pure result
  where
    readLines: File -> (n ** Vect n String) -> IO (m ** Vect m String)
    readLines f (n ** lines) = do 
        False <- fEOF f
            | True => pure (n ** lines)
        Right line <- fGetLine f
            | Left err => pure (n ** lines)
        readLines f ((n + 1) ** (lines ++ [line]))



