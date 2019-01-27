module Main

main : IO ()
main = do putStr "First String: "
          s1 <- getLine
          putStr "Second String: "
          s2 <- getLine
          let l1 = length s1
          let l2 = length s2
          putStrLn (show (max l1 l2))
