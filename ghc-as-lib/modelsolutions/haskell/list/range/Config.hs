{-# DESC
Write a function which enumerates all numbers contained in a given range.
You can assume the second number is bigger then the first.

range :: Int -> Int -> [Int]

For example:

> range 2 5
[2,3,4,5]
#-}

module Config where

import Test.QuickCheck

properties :: [(Int -> Int -> [Int]) -> Property]
properties = [propLength, propEnums, propStart]

forSmallPair = forAll $ do
    x <- choose (-100, 100)
    y <- choose (0,    100)
    return (x, x + y)

propLength f = 
    counterexample "The result is not of the right length" $ 
      forSmallPair $ \(x, y) -> 
        length (f x y) == abs (x - y) + 1

propEnums f = 
    counterexample "The result is not an increasing list" $ 
      forSmallPair $ \(x, y) -> 
        let xs = f x y in and $ zipWith (\a b -> succ a == b) xs (tail xs)
        
propStart f = 
    counterexample "The result does not begin with the right value" $ 
      forSmallPair $ \(x, y) -> x == head (f x y)
