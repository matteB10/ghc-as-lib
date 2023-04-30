{-# DESC 
Find the maximum element of a list:

mymax :: [Int] -> Maybe Int 

For example:

> mymax [1,100,33,0]
Just 100 

> mymax []
Nothing 
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> Maybe Int) -> Property]
properties = [propModel]

propModel f = property $ \xs -> f xs == max xs

max [] = Nothing 
max xs = Just $ max' 0 xs 
    where max' m [] = m 
          max' m (x:xs) | x > m     = max' x xs
                        | otherwise = max m xs  
