-- Q16, drop every Nth element from a list

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery myList k = dropEveryHelper myList k 1

dropEveryHelper :: [a] -> Int -> Int -> [a]
dropEveryHelper [] _ _ = []
dropEveryHelper (current : rest) drop count
  | count `mod` drop == 0 = dropEveryHelper rest drop (count + 1)
  | otherwise = [current] ++ dropEveryHelper rest drop (count + 1)