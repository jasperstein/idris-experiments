module Ex9counts

import Ch2sec4.Ex1to8

Main.main : IO ()
Main.main = repl "\nEnter a String: " (\input => (show (counts6 input)))
