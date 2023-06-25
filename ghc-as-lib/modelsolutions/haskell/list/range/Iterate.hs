module Iterate where

range :: Int -> Int -> [Int]

{-# FC This approach makes clever use of the functions already defined in the @prelude,
       which is in general good coding practice.
#-}
range l u = take (1 + u - l) (iterate (+1) l)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}