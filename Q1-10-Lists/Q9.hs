-- Q9, packing duplicates into sublists
pack :: (Eq a) => [a] -> [[a]]
pack (x : xs) = packHelper (xs) x 0

packHelper :: (Eq a) => [a] -> a -> Int -> [[a]]
packHelper (x : xs) y count
  | xs == [] && x == y = [replicate (count + 1) y]
  | xs == [] && x /= y = [replicate (count + 1) y] ++ [[x]]
  | x == y = packHelper (xs) y (count + 1)
  | x /= y = [replicate (count + 1) y] ++ packHelper (xs) x 0