{-# OPTIONS_GHC -Wno-typed-holes #-}


module Mod1 where 
    
dupli :: [a] -> [a]
dupli = foldr (\x xs -> x:x:xs) [] 
