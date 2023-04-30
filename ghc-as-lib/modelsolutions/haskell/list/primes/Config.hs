{-# DESC
Define the list of all prime numbers

primes :: [Int]

Such that:

> take 5 primes
[2,3,5,7,11]
#-}

module Config where

import Test.QuickCheck

properties :: [[Int] -> Property]
properties = [propPrime]

propPrime xs = property $ and $ take 100 $ zipWith (==) xs primes
  where
    primes = let rec (x:xs) = x : [y | y <- rec xs, y `mod` x /= 0] 
             in  rec [2..]
