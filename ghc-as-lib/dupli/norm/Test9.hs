{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test9 where 


dupli :: [a] -> [a]
dupli xs = concatMap _ xs 

