module Utils.String where 

import Data.Char (isSpace)

banner :: [Char] -> IO ()
banner msg = putStrLn $ "\n\n--- " ++ msg ++ " ---\n\n"

-- String concatenation helpers
sp, nl, cm :: String -> String -> String
sp x y = x ++ " " ++ y
nl x y = x ++ "\n" ++ y
cm x y = x ++ " , " ++ y

strip :: String -> String
strip = compress . lstrip . rstrip

lstrip :: String -> String
lstrip [] = []
lstrip (x:xs) | isSpace x = lstrip xs
              | otherwise      = x:xs

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

compress (x : y : xs) = if x == y && isSpace x then compress xs else x : compress (y : xs)
compress x = x