module Transpose3 where

transpose :: [a] -> [a]
{-# DESC Travel the number of columns and fetch the right cells #-}

transpose rs = cols 0
 where
  max = length (head rs)
  cols c | c < max   = map (!! c) rs : cols (c+1)
         | otherwise = []

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
