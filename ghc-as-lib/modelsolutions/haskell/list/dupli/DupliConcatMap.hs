module DupliConcatMap where

dupli :: [a] -> [a]
{-# ETADECL 1 #-}

dupli = {-# F @concatMap first maps a function over a @list and then concatenates the result. 
        #-} concatMap {-# ETA 1 #-} (replicate 2)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}