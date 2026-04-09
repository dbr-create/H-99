-- Q1, Getting the last element in a list
myLast :: [a] -> a
myLast (x : []) = x
myLast (x : xs) = myLast xs