{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 

-- Let-rec problem 

--factorial :: (Eq t, Num t) => t -> t
factorial n = f n 
    where f 0 = 1 
          f m = m * f (m - 1)