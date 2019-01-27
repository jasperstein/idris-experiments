module Main

main : IO ()
main = putStr "First String: " >>= \_ =>
       getLine >>= \s1 =>
       putStr "Second String: " >>= \_ =>
       getLine >>= \s2 =>
       let l1 = length s1 in
       let l2 = length s2 in
       putStrLn (show (max l1 l2))
