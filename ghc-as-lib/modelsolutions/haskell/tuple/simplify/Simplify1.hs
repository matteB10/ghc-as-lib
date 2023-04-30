module Simplify1 where

simplify :: (Int, Int) -> (Int, Int)
{-# DESC Use the @prelude functions: @gcd, @signum, @abs and @div #-}

simplify (n, d) = ((signum d * n) `div` g, abs d `div` g)
    where   g = gcd n d

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
