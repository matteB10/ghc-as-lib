module Encode2 where

encode :: Eq a => [a] -> [(Int, a)]
{-# DESC Use the @higher-order function @foldr. #-}

encode = foldr f []
 where
   f x [] = [(1, x)]
   f x ((n, y) : zs) | x == y    = (n+1, y) : zs
                     | otherwise = (1,   x) : (n, y) : zs

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
