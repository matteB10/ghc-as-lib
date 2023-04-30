module Solution163 where

dropevery :: [a] -> Int -> [a]
{-# DESC @tuple every listelement with 1 to n, and @filter away every n-th element. #-}

{-# FC A solution without @pattern-matching is often preferred. #-}
dropevery xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
