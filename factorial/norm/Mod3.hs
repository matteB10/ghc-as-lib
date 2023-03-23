{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod3 where 


factorial :: Integer -> Integer 
factorial 0 = 1 
factorial m = m * factorial (m - 1)  
