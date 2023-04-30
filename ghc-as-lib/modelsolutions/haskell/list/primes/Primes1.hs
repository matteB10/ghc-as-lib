module Primes1 where

primes :: [Int]
{-# DESC Use @prelude @filter and @rem and define a helper function `isPrime' #-}

primes = filter isPrime [1..]
    where   isPrime x = filter (isDivisorOf x) [1..x] == [1,x]
            isDivisorOf x y = rem x y == 0

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
