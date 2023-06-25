module Encode4 where

encode :: Eq a => [a] -> [(Int, a)]

encode []     = []
encode (x:xs) = (n, x) : encode rest
    where n = length eqx + 1
          (eqx, rest) = span (== x) xs

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}