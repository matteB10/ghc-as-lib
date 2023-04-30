{-# DESC
Replicate the elements of a list a given number of times:

repli :: [a] -> Int -> [a]

For example:

> repli "abc" 3
"aaabbbccc"
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Int -> [Int]) -> Property]
properties = [propModel]

propModel f = 
    forAll (choose (0, 100)) $ \n -> 
    forAll arbitrary $ \xs ->
      concatMap (replicate n) xs == f xs n
