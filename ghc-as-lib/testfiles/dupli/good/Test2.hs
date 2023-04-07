{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 


dupli :: [a] -> [a]
dupli xs = hole doub xs 
    where doub = replicate 2 
          hole = _ 


