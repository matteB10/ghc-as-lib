{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 


factorial :: (Eq t, Num t) => t -> t
factorial n = f n 
    where f 0 = 1 
          f m = m * f (m - 1)