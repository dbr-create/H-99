-- Q18, extracting a slice from a list

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (current : rest) low high
  | low > 1 = slice rest (low - 1) (high - 1)
  | high > 1 = [current] ++ slice rest (low) (high - 1)
  | otherwise = [current]