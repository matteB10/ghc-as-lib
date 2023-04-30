module Primes3 where

primes :: [Int]
{-# DESC Implement @eratosthenes-sieve by using explicit recursion and @filter #-}

primes = sieve [2 .. ]
  where sieve (p:xs) = p : sieve (filter (\x -> mod x p /= 0) xs)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
