module Ex1to8

a1: ()
a1 = let x = the (String, String, String) ("A", "B", "C")
         y = the (List String) ["A", "B", "C"]
         z = the ((Char, String), Char) (('A', "B"), 'C') in ()

export palindrome2: String -> Bool
palindrome2 x = x == reverse x

palindrome3: String -> Bool
palindrome3 x = let l = toLower x in palindrome2 l

palindrome4: String -> Bool
palindrome4 x = palindrome2 x && length x > 10

palindrome5: Nat -> String -> Bool
palindrome5 n x = palindrome2 x && length x > n

export counts6: String -> (Nat, Nat)
counts6 input = (length (words input), length input)

top_ten7: Ord a => List a -> List a
top_ten7 input = take 10 (reverse (sort input))

over_length8: Nat -> List String -> Nat
over_length8 ln inputs = length (filter (\input => (length input > ln)) inputs)
