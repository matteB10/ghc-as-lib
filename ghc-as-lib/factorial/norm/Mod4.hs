{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod4 where 


--factorial :: Integer -> Integer 
factorial :: Integer -> Integer
factorial = f 
    where f :: Integer -> Integer 
          f 0 = 1 
          f m = m * f (m - 1)    


