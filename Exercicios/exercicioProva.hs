quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (x:xs) = quicksort [y| y <- xs, y <= x] ++ [x] ++ quicksort [y| y <- xs, y > x]

f :: (Ord t) => [t] -> [t] -> [t]
f l1 l2 = removeDuplicates newList
     where newList = quicksort(l1++l2)

removeDuplicates :: (Eq t) => [t] -> [t]
removeDuplicates [] = []
removeDuplicates (x:xs) = [x] ++ removeDuplicates[y| y <- xs, x /= y]