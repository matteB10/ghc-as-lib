{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod8 where 


dupli :: [a] -> [a]
dupli xs = concatMap (replicate 2) xs 

