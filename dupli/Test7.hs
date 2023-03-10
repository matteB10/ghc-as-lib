{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test7 where 

dupli :: [a] -> [a]
dupli xs = concatMap (\x -> [x,x]) xs  


{- test :: a -> [a] -> [a]
test x xs = x : xs  -}

{- f :: (Int -> Int -> Int) -> Int -> Int -> Int
f g x y = g x y  
 -}
