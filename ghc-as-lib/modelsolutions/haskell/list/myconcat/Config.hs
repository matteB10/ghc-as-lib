{-# DESC
Write a function that flattens a list:

myconcat :: [[a]] -> [a]

For example:

> myconcat [[1,2],[3,4]]
[1,2,3,4]
#-}

module Config where

import Test.QuickCheck

properties :: [([[Int]] -> [Int]) -> Property]
properties = [propModel]

propModel f = property $ \xss -> f xss == concat xss
