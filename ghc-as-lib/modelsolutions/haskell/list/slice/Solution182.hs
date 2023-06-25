module Solution182 where

slice :: [a] -> Int -> Int -> [a]

slice lst 1 m = slice' lst m []
  where
    slice' _      0 acc = reverse acc
    slice' (y:ys) n acc = slice' ys (n - 1) (y:acc)
slice (x:xs) n m = slice xs (n - 1) (m - 1)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}