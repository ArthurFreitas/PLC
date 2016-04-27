ordena :: (Ord t) => [t] -> [t]
ordena (x:xs) = ordena [y | y <- xs, y <= x] ++ [x] ++ ordena [y | y <- xs, y > x]
ordena _ = []

agrupar :: (Eq t) => [[t]] -> [(t,Int)]
agrupar ((y:ys):xs) = 

count x [] = 0
count x (y:ys) 
    | x == y = 1 + count x (ys)
	| x /= y = count x (ys)				
 
