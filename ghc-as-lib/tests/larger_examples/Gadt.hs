module Gadt where 

data Point = Point {x :: Int, y :: Int}

data Funs = Funs {sin' :: Double -> Double, cos' :: Double -> Double}


testPoint :: Point
testPoint = Point {x = 1, y = 2}

sinfun = Prelude.sin 
cosfun = Prelude.cos 

funs = Funs {sin' = sinfun, cos' = cosfun}

