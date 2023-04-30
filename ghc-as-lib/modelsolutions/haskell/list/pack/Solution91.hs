module Solution91 where

pack :: Eq a => [a] -> [[a]]
{-# DESC Use explicit @recursion and the @prelude function @span. #-}

pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs
              in (x:first) : pack rest

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
