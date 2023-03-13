{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod2 where

dupli :: [a] -> [a]
dupli xs = concatMap _ xs   