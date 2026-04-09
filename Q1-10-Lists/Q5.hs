-- Q5, Reversing a List
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse (xs) ++ [x]