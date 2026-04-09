-- Q2, getting the second to last element in a List
secondToLast :: [a] -> a
secondToLast (x : xs) = secondToLastHelper xs x

secondToLastHelper :: [a] -> a -> a
secondToLastHelper (x : []) y = y
secondToLastHelper (x : xs) y = secondToLastHelper xs x