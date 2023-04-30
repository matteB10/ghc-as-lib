module Solution95 where

pack :: Eq a => [a] -> [[a]]
{-# DESC Use explicit @recursion with @guards #-}

pack []  = []
pack [x] = [[x]]
pack (x:xs) | x == head r = (x:r):rs
            | otherwise   = [[x]] ++ res
   where res@(r:rs) = pack xs

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
