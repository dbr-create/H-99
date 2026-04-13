-- Q19, rotate a list k places

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate (current : rest) count
  | count < 1 = (current : rest)
  | y > 0 = rotate (rest ++ [current]) (count - 1)
