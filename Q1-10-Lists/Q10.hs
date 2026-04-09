-- Q10, run length encoding for a list
encode :: (Eq a) => [a] -> [(Integer, a)]
encode (x : xs) = encodeHelper (xs) x 1

encodeHelper :: (Eq a) => [a] -> a -> Integer -> [(Integer, a)]
encodeHelper (current : rest) last count
  | rest == [] && current == last = [(count + 1, last)]
  | rest == [] && current /= last = [(count, last)] ++ [(1, current)]
  | current == last = encodeHelper rest current (count + 1)
  | current /= last = [(count, last)] ++ encodeHelper rest current 1