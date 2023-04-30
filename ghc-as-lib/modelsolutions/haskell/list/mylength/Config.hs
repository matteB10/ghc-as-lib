{-# DESC
Write a function that calculates the number of elements a list contains:

mylength :: [a] -> Int

For example:

> mylength [1,2,3]
3
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Int) -> Property]
properties = [propModel]

propModel f = property $ \xs -> f xs == length xs
