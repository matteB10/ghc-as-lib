{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where 


factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1 
factorial m = m * factorial (m - 1)    