module AllPairsConcatMapMap where

allpairs :: [a] -> [(a,a)]

allpairs xs = concatMap pair xs
  where 
    pair x    = map (pair2 x) xs
    pair2 x y = (x, y)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}