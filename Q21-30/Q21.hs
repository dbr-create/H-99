-- Q21, insert a value X as position k in a list

insertAt :: (Eq a) => a -> [a] -> Integer -> [a]
insertAt value [] pos = [value]
insertAt value (current : rest) pos
  | pos == 1 = [value] ++ [current] ++ rest
  | otherwise = [current] ++ insertAt value (rest) (pos - 1)