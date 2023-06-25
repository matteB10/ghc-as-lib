module Identity2 where

identity :: Num a => a -> b

{-# FC Define the identity function using @unfoldr. Use @unfoldr to create a row 
       with a one at the right place, by using the row number as the seed.
       Subsequently map this over a list with all row numbers. #-}
identity n = map (\i -> unfoldr (f i) 1) [1 .. n]
  where 
    f i b | b > n     = Nothing 
          | b == i    = Just {-# F Introduce an expression that gives the output value and the value for the 
next iteration. #-} (1, b + 1)
          | otherwise = Just (0, b + 1)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}