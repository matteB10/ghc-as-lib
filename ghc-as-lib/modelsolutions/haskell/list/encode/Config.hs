{-# DESC
Write a function that calculates the so-called `run-length encoding' of a list:

encode :: Eq a => [a] -> [(Int, a)]

For example:

> encode [1,2,2,3,2,4]
[(1,1),(2,2),(1,3),(1,2),(1,4)]
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> [(Int, Int)]) -> Property]
properties = [propModel]

propModel f = property $ \xs -> f xs == encode xs

encode xs = [(length ys, y) | ys@(y:_) <- group xs]

group []       = []
group xs@(x:_) = let (ys, zs) = span (== x) xs
                 in ys : group zs
