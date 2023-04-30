module Recursion where

range :: Int -> Int -> [Int]
{-# DESC Implement range using direct @recursion #-}

{-# FC 
  Using direct @recursion means we have to explicitly look at the arguments and decide what to do. Because
  the condition concerns both the first and second parameter, we use @guards.
#-}
range l u | l == u     = [l]
          | otherwise  = l : range (l + 1) u

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
