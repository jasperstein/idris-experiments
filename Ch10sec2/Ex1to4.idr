module Ex1to4

import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc rec) with (snocList ys)
    equalSuffix (zs ++ [x]) [] | (Snoc rec) | Empty = []
    equalSuffix (xs' ++ [x]) (ys' ++ [y]) | (Snoc xs'rec) | (Snoc ys'rec) = 
        if (x/=y) then [] else (equalSuffix xs' ys' | xs'rec | ys'rec) ++ [x]

mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (ys ++ zs) | (SplitRecPair lrec rrec) = merge (mergeSort ys | lrec) (mergeSort zs | rrec)

toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = (toBinary n | rec) ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = (toBinary n | rec) ++ "1"

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = (x==y) && palindrome ys | rec
