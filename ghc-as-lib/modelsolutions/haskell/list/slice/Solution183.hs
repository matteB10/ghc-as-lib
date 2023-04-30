module Solution183 where

slice :: [a] -> Int -> Int -> [a]
{-# DESC  Recurse over the list argument and the integers. #-}

slice (x:xs) i k
   | i > 1     = slice xs (i - 1) (k - 1)
   | k <= 1    = [x]
   | otherwise = x:slice xs (i - 1) (k - 1) 

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
