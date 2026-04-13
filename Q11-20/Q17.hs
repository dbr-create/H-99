-- Q17, split a list into two parts given the length of the first par

split :: [a] -> Integer -> ([a], [a])
split myList y
  | y > 0 = splitHelper myList [] y
  | y <= 0 = ([], myList)

splitHelper :: [a] -> [a] -> Integer -> ([a], [a])
splitHelper [] current _ = (current, [])
splitHelper (x : xs) current 1 = (current ++ [x], xs)
splitHelper (x : xs) current y = splitHelper xs (current ++ [x]) (y - 1)