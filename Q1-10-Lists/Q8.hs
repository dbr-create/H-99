-- Q8, List Compression
compress :: (Eq a) => [a] -> [a]
compress (x : xs) = [x] ++ compressHelper xs x

compressHelper :: (Eq a) => [a] -> a -> [a]
compressHelper (x : xs) y | xs == [] && x == y = []
compressHelper (x : xs) y | xs == [] && x /= y = [x]
compressHelper (x : xs) y | x == y = compressHelper xs y
compressHelper (x : xs) y | x /= y = [x] ++ compressHelper xs x
