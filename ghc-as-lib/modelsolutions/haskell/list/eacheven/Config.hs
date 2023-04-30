{-# DESC 
Check whether each element is even, in order

eacheven :: [Int] -> [Bool]

For example:

> dupli [1,2,3]
[False, True, False]
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> [Int]) -> Property]
properties = [propModel]

propModel f = property $ \xs -> f xs == map even xs
