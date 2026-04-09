-- Q4, Getting the length of a List
myLength :: [a] -> Integer
myLength [] = 0
myLength (x : xs) = myLength (xs) + 1