{-# DESC 
Duplicate the elements of a list:

dupli :: [a] -> [a]

For example:

> dupli [1,2,3]
[1,1,2,2,3,3]
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> [Int]) -> Property]
properties = [propModel]

propModel f = property $ \xs -> f xs == concatMap (replicate 2) xs
