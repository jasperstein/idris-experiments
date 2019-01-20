module Ex9palindrome

import Ch2sec4.Ex1to8

Main.main: IO ()
Main.main = repl "\nEnter a String: " (\input => (show (palindrome2 input)))
