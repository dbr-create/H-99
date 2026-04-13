-- Q15, replicate all values in a list by a value k

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) y = replicate y x ++ repli xs y