{-# DESC
Remove the n-th element from a list , with n >= 1.
Start counting from 1.

removeat :: Int -> [a] -> (Maybe a, [a])

For example:

> removeat 2 [1,2,3,4,5]
(Just 2,[1,3,4,5])
#-}

module Config where

import Test.QuickCheck

properties :: [(Int -> [Int] -> (Maybe Int, [Int])) -> Property]
properties = [propAnswer, propRest]

propAnswer f = 
    counterexample "The removed value is not the correct one" $
      forAll arbitrary $ \xs ->
      forAll (choose (1, length xs)) $ \n -> fst (model n xs) == fst (f n xs)

propRest f = 
    counterexample "The given result-list is not correct" $
      forAll arbitrary $ \xs ->
      forAll (choose (1, length xs)) $ \n -> snd (model n xs) == snd (f n xs)

model n xs | n > 0 && n <= length xs = let (ys, z:zs) = splitAt (n-1) xs 
                                       in (Just z, ys ++ zs)
           | otherwise               = (Nothing, xs)
  
