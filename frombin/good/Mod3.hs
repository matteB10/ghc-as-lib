{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod3 where 


fromBin :: Num a => [a] -> a
fromBin = foldl op 0
    where 
      n `op` b = 2*n + b 