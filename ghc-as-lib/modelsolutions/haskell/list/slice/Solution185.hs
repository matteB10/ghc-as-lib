module Solution185 where

slice :: [a] -> Int -> Int -> [a]
{-# DESC @tuple each @list element with an integer denoting its position, and then @filter away the elements outside the range. #-}

slice xs i j = map snd
             $ filter (\(x,_) -> x >= i && x <= j)
             $ zip [1..] xs
{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
