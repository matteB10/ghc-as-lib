module Solution83 where

compress :: Eq a => [a] -> [a]

{-# FC  A function which drops elements from a @list can be found in the @prelude. #-}

compress []     = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}