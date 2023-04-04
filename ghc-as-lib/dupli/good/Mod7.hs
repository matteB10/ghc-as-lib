{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod7 where


dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs