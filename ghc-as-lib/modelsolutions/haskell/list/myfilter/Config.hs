{-# DESC
Write a function that filters a list:

myfilter :: (a -> Bool) -> [a] -> [a]

For example:

>myfilter even [1..10]
[2,4,6,8,10]
#-}

module Config where

import Test.QuickCheck

properties :: [((Int -> Bool) -> [Int] -> [Int]) -> Property]
properties = [propPred, propPresent] 

-- It is possible to generate predicate functions using QC, but they are
-- relatively slow. So, left them out for now.

propPred f = 
    counterexample "The result contains elements for which the given predicate does not hold" $
      {-\p xs -> and $ map (apply p) (f (apply p) xs)-}
      \xs -> and $ map even (f even xs)

propPresent f = 
    counterexample "Too many elements were removed" $
      {-\p xs -> and $ map (\x -> flip elem (f (apply p) xs) x || not ((apply p) x)) xs-}
      \xs -> and $ map (\x -> flip elem (f even xs) x || not (even x)) xs
