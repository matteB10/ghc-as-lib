{-# DESC
Rotate a list n places to the left:

rotate :: [a] -> Int -> [a]

For example:

> rotate [1,2,3,4,5] 2
[3,4,5,1,2]

Note: n might be negative, or larger than the length of the list.
#-}

module Config where

import Data.List
import Test.QuickCheck

properties :: [([Int] -> Int -> [Int]) -> Property]
properties = [propLength, propElems, propModelNeg, propModelBig, propModelRange]

propLength f = 
    counterexample "The result is not of the right length" $ \xs n -> 
      length xs == length (f xs n)

propElems f = 
    counterexample "The result does not contain the same values as the input" $ \xs n -> 
      sort xs == sort (f xs n)

propModelNeg f = 
    counterexample "The rotation is not applied correctly for negative input" $ \xs -> 
      forAll (choose (-3 * (length xs), 0)) $ \n -> f xs n == model xs n

propModelBig f = 
    counterexample "The rotation is not applied correctly for input larger then the input list" $ \xs ->
      let n = length xs in forAll (choose (n, n*3)) $ \n -> f xs n == model xs n

propModelRange f =
    counterexample "The rotation is not applied correctly" $ \xs ->
      forAll (choose (0, length xs)) $ \n -> f xs n == model xs n

model xs n 
    | null xs   = xs 
    | otherwise = let (ys, zs) = splitAt (n `mod` length xs) xs in zs ++ ys
