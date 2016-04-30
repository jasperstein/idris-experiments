import range

board : List (Integer, Integer)
board = range2 (1,1) (8,8)

main : IO ()
main = putStrLn $ show board