module Mod7 where 

myreverse :: [a] -> [a]
myreverse = reverse' []
  where
    --reverse' :: [a] -> [a] -> [a] 
    -- will not match if given a type explicitly, create an extra type lambda
    -- see Mod3
    reverse' acc [] = acc
    reverse' acc (x:xs) = reverse' (x:acc) xs