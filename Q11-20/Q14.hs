-- Q14, Duplicate elements in a list

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = [x, x] ++ dupli xs
