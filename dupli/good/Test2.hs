{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 

dupli :: [a] -> [a]
dupli xs = _ (replicate 2) xs 
