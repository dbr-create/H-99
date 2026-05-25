-- Q26, generating all combiations (not permutations) of N elements from a list

combinations :: Integer -> [a] -> [[a]]
combinations count (x : xs) = combinationsHelper count (x : xs) []

combinationsHelper :: Integer -> [a] -> [a] -> [[a]]
combinationsHelper 0 myList accumulator = [accumulator]
combinationsHelper count myList accumulator = concat [combinationsHelper (count - 1) (drop (x + 1) myList) (accumulator ++ [y]) | (x, y) <- zip [0 ..] myList]
