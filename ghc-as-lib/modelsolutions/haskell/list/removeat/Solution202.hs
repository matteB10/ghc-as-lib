module Solution202 where

removeat :: Int -> [a] -> (Maybe a, [a])

removeat n [] = (Nothing,[]) 
removeat n (x:xs) = if n==1 then (Just x,xs) else let (ma,rs) = removeat (n-1) xs in (ma,x:rs)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}