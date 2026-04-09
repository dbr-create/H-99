-- Q3, getting an arbitrary k-th element in a list
elementAt :: [a] -> Integer -> a
elementAt (x : xs) 1 = x
elementAt (x : xs) y = elementAt xs (y - 1)