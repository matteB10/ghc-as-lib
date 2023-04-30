module Solution84 where

compress :: Eq a => [a] -> [a]
{-# DESC @pattern-match on the empty @list, a singleton list, and lists with at least two elements. #-}

compress []        =  []
compress [x]       =  [x]
compress (x:y:ys)  =  if x==y then compress (x:ys) else x:compress (y:ys)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
