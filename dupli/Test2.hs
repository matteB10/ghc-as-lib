{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 

dupli :: [a] -> [a]
dupli ys = _ (replicate 2) ys 
