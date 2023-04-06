module Test7 where 

myreverse :: [a] -> [a]
myreverse = reverse' [] 
  
reverse' acc [] = acc
reverse' acc (x:xs) = reverse' (x:acc) xs
