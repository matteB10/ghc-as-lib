module Encode3 where

encode :: Eq a => [a] -> [(Int, a)]
{-# DESC @tuple each @list element with the integer 1, and merge the elements. #-}

encode xs = merge $ map (\z -> (1, z)) xs
 where merge [] = []
       merge [(n,x)] = [(n,x)]
       merge ((m,x):(n,y):rest) = if x==y
                                  then merge ((m+n,x):rest)
                                  else (m,x):merge ((n,y):rest)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
