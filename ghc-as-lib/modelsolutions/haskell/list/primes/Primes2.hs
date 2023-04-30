module Primes2 where

primes :: [Int]
{-# DESC Implement @eratosthenes-sieve by using @iterate and @filter from @prelude #-}

primes = (map head (iterate multiples [2..]))
    where 
        multiples (x:xs) = filter (isNotDivBy x) xs
        isNotDivBy x y  = rem y x /= 0

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
