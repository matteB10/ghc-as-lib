{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod8 where 


dupli :: [a] -> [a]
dupli = _ (replicate 2) 

