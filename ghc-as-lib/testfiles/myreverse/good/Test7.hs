module Test7  where 

myreverse :: [a] -> [a]
myreverse = reverse' [] 
  
reverse' :: [a] -> [a] -> [a]
reverse' acc [] = acc
reverse' acc (x:xs) = reverse' (x:acc) xs

