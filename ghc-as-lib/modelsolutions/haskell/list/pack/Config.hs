{-# DESC
Write a function that groups consecutive duplicates of list elements into sublists.

pack :: Eq a => [a] -> [[a]]

If a list contains repeated elements they should be placed in separate sublists.

For example:

> pack [2,2,1,3,3,3]
[[2,2],[1],[3,3,3]]
#-}

module Config where

import Test.QuickCheck

type Fun = [Int] -> [[Int]]

properties :: [Fun -> Property]
properties = [propConcat, propNeighbours, propSubSets]

propConcat f = 
    counterexample "Concatenation of the result does not give the input list" $
      \xs -> concat (f xs) == xs

propNeighbours f = 
    counterexample "Subsequent sublists can not contain the same elements" $
      \xs -> let nb (x:y:ys) = (not $ elem (head x) y) && nb (y:ys)
                 nb _        = True
             in nb $ f xs

propSubSets f = 
    counterexample "Not every sublist contains only copies of the same element" $ 
      let p (x:y:ys) = x == y && p (y:ys) ; p _ = True in and . map p . f
