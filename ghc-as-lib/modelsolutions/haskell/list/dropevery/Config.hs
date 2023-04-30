{-# DESC 
Drop every n-th element from a list:

dropevery :: [a] -> Int -> [a]

> dropevery [0,1,2,3,4,5,6,7,8,9] 3
[0,1,3,4,6,7,9] 
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Int -> [Int]) -> Property]
properties = [propSubSet, propLength1, propLength2, propModel]

propSubSet f = 
    counterexample "The resulting list is not a subset of the input list" $
      \xs n -> and $ map (flip elem xs) (f xs n)

propLength1 f = 
    counterexample "You have removed too many elements" $ 
      \xs n -> n > 0 ==> length (f xs n) >= length xs - div (length xs) n

propLength2 f = 
    counterexample "You have removed too few elements" $
      \xs n -> n > 0 ==> length (f xs n) <= length xs - div (length xs) n

propModel f = 
    counterexample "You have not removed the right elements" $ 
      \xs n -> de xs n == f xs n
  where
    de xs n = case splitAt (n-1) xs of 
                (ys, [])     -> ys
                (ys, (z:zs)) -> ys ++ de zs n
