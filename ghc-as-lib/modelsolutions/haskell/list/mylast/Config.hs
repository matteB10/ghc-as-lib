{-# DESC 
Write a function that selects the last element of a list:

mylast :: [a] -> a

For example:

> mylast [1,2,3,4]
4
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Int) -> Property]
properties = [propModel]

propModel f = property $ \xs -> f xs == last xs
