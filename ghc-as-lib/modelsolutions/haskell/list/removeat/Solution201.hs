module Solution201 where

removeat :: Int -> [a] -> (Maybe a, [a])

removeat k xs = 
   let (front, back) = splitAt (k-1) xs 
   in case back of
         []     -> (Nothing, front)
         x:rest -> (Just x, front ++ rest)
  

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}