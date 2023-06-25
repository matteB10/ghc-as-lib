module Myuncurry where

myuncurry :: (a -> b -> c) -> (a, b) -> c

{-# FC When looking at the function description we speak of the function as if only having 1 parameter, however the result function has an argument itself which we can also use for pattern matching. We can easily see this when looking at the type signature of the function. A simple yet effective technique #-}

myuncurry f (x,y) = f x y

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}