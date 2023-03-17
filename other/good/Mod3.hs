module Mod3 where 


f :: [Int] -> Int
f [] = 1
f [2] = 2
f [3] = 3
f ds = if length ds == 2 then 42 else 4 