{-# DESC
Write a function that creates all the possible pairs of the items of a list.
The key is to use the input list twice.
You can assume that the input does not contain duplicates.

allpairs :: [a] -> [(a,a)]

For example:

> allpairs [1..3]
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
#-}

module Config where

import Prelude hiding (concatMap)
import GHC.OldList
import Test.QuickCheck

check :: ([Int] -> [(Int, Int)]) -> IO Result 
check = quickCheckResult . propModel
    
properties :: [([Int] -> [(Int, Int)]) -> Property]
properties = [propModel]

propModel :: ([Int] -> [(Int, Int)]) -> Property
propModel f = forAll arbitrary $ \xs -> nub xs == xs ==>   -- perhaps better to use other generator
    sort (f xs) == sort [(x, y) | x <- xs, y <- xs]
