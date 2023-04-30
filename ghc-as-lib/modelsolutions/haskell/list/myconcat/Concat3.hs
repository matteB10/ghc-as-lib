module Concat3 where

myconcat :: [[a]] -> [a]
{-# DESC Use explicit @recursion. #-}

{- deze oplossing zou met een standaardstrategie voor foldr en ++ moeten worden herkend
myconcat []       = []
myconcat (xs:xss) = foldr (:) (myconcat xss) xs
-}

myconcat [] = []
myconcat (xs:xss) = xs ++ myconcat xss

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
