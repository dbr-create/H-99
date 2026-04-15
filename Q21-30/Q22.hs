-- Q22, giving an inclusive range between two values
range :: Integer -> Integer -> [Integer]
range low high
  | low > high = []
  | low < high = [low] ++ range (low + 1) high
  | low == high = [low]