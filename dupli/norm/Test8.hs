{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test8 where 



dupli :: [a] -> [a]
dupli = _ (replicate _)



