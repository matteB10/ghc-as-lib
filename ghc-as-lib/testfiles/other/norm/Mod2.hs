module Mod2 where 

-- test rewrite rules pragmas 
addAndMult :: Int -> [Int] -> [Int]
addAndMult n = map ((*n) . (+n)) 