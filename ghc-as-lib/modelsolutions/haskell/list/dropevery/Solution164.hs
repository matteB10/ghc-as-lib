module Solution164 where

dropevery :: [a] -> Int -> [a]

dropevery xs n = snd $ foldl (\acc e -> if fst acc > 1 then (fst acc - 1, snd acc ++ [e]) else (n, snd acc)) (n, []) xs
{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}