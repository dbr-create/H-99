-- Q49, n bit gray codes

gray :: Int -> [String]
gray num = grayHelper ["0", "1"] num

grayHelper :: [String] -> Int -> [String]
grayHelper accumulator num
  | (length accumulator) == (2 ^ num) = accumulator
  | otherwise = grayHelper ((map ("0" ++) accumulator) ++ (map ("1" ++) (reverse accumulator))) num