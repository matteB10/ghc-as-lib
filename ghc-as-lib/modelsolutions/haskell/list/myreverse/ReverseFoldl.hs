module ReverseFoldl where

myreverse :: [a] -> [a]
{-# DESC Use the @prelude function @foldl. #-}


myreverse = 
  {-# F Since we want to store the partially reversed list 
        in an accumulating parameter, we use the function @foldl 
        here, which essentially maintains an accumulating 
        parameter.
   #-}  (foldl
           {-# F We want to cons the next element to the
                   accumulated list. The first argument of the
                   function of the fold is the accumulated 
                   parameter, and the second the next element.
                   We hence have to @flip these arguments before
                   we pass them to (:).
           #-}(flip (:))
           []
         )


{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
