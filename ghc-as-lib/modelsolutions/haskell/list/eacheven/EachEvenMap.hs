module EachEvenMap where

eacheven :: [Int] -> [Bool]
{-# DESC Use the @prelude function @concatMap. #-}      
{-# ETADECL 1 #-}

eacheven = map {-# ETA 1 #-} even

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
