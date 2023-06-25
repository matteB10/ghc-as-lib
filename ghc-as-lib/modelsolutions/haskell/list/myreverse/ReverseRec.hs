module ReverseRec where

myreverse :: [a] -> [a]

myreverse [] = []
{-# FC @append can be used to put an item at the end of a list. #-}
myreverse (x:xs) = (myreverse xs ++ [x])

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}