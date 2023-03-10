{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test5 where 

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = _ ++ dupli xs  
    --where dup = _

