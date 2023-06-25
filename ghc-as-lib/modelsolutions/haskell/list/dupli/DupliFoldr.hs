module DupliFoldr where

dupli :: [a] -> [a]
{-# ETADECL 1 #-}

{-# FC It is often considered good coding practice to use the recursion combinators @foldr or @foldl. #-}
dupli = foldr (\ x xs -> x : x : xs) []

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}