{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test9 where 

dupli :: [b] -> [b]
dupli xs = concat $ map _ xs
