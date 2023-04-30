module Solution162 where

dropevery :: [a] -> Int -> [a]
{-# DESC Use the @prelude functions @take and @drop. #-}

dropevery [] _ = []
dropevery list count = (take (count-1) list) ++ dropevery (drop count list) count

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
