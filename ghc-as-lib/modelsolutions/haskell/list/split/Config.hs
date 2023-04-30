{-# DESC
Split a list into two parts; the length of the first part is given:

split :: [a] -> Int -> ([a], [a])

For example:

> split [1,2,3,4,5] 3
([1,2,3],[4,5])
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Int -> ([Int], [Int])) -> Property]
properties  = [propLength, propMerge]

propLength f = 
    counterexample "The length of first result subset is not correct" $ \xs ->
      forAll (choose (0, length xs)) $ \n -> fst (f xs n) == fst (splitAt n xs)
      
propMerge f = 
    counterexample "The result is not a correct split" $ \xs ->
      forAll (choose (-1, length xs)) $ \n -> 
        let (ys, zs) = f xs n in xs == ys ++ zs 
