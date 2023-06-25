module Acc where

fromBin :: [Int] -> Int

fromBin bs = fromBin' (length bs - 1) bs
  where
    fromBin' _ []     = 0
    fromBin' n (b:bs) = 2^n * b + fromBin' (n-1) bs

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}