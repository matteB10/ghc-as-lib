{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod9 where 


dupli :: [a] -> [a]
dupli = concat . map (\x -> [x,x]) 

