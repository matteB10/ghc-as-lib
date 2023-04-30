{-# DESC
Write a function that finds the n-th element of a list

elementat :: [a] -> Int -> a

The first element in the list has number 1. You may assume that
1 <= n <= the length of the input list.

For example:

> elementat [2,2,4,1] 3
4
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Int -> Int) -> Property]
properties = [propModel]

propModel f = property $ \xs -> let n = length xs in
    n > 0 ==> forAll (choose (1, n)) $ \m -> f xs m == xs !! (m-1)
