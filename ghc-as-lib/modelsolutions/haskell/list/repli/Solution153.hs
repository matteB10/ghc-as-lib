module Solution153 where

repli :: [a] -> Int -> [a]
{-# ASS natural number > 0 #-}

repli xs n = repli' xs n []
    where repli' []     _ acc = acc
          repli' (x:xs) 0 acc = repli' xs n acc
          repli' (x:xs) n acc = repli' (x:xs) (n-1) (acc++[x])

{-# FC Calling repli with an (n < 0) will result in an infinite loop. We can add a guard on n to prevent this from happening. Note that the recursion on the natural number behaves just like the prelude function @replicate. The way the accumulated result is used resembles @foldl. Both these functions can be used in alternative (easier, more efficient) solutions to this problem. #-}


{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}