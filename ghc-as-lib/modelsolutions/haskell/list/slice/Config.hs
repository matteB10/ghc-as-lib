{-# DESC
Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]

Given two indices, i and k, the slice is the list containing the elements 
between the i'th and k'th element of the original list (both limits included). 
Start counting the elements with 1.

For example:

> slice [1,2,3,4,5,6,7,8,9] 4 6
[4,5,6]
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Int -> Int -> [Int]) -> Property]
properties = [propLength, propModel]

{-prop_Main = \xs i j -> i > 0 && length xs > 0 ==> -}
  {-let {j' = max j (i+1); xs' = take (max (length xs) j') $ cycle xs;} in prop_Length xs' i j' .&&. prop_Elem xs' i j' .&&. prop_Model xs' i j'-}

indexesGen :: [a] -> Gen (Int, Int) 
indexesGen xs = do 
    n <- choose (1, length xs)
    m <- choose (n, length xs)
    return (n, m)

propLength f = 
    counterexample "The resulting slice is not of the right length" $ \xs ->
      xs /= [] ==> forAll (indexesGen xs) $ \(n, m) -> length (f xs n m) == m - n + 1

propModel f =
    counterexample "You have not selected the right elements" $ \xs ->
      forAll (indexesGen xs) $ \(n, m) -> f xs n m == model xs n m

model xs n m = take (m - n + 1) $ drop (n - 1) xs 
