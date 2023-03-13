{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test3 where 

fromBin bits = foldl (\acc bit -> acc * 2 + bit) 0 bits