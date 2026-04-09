-- Q11, modified RLE
data RLE a = Single a | Multiple Int a
  deriving (Show)

encodeModified :: (Eq a) => [a] -> [RLE a]
encodeModified [] = []
encodeModified (current : []) = [Single current]
encodeModified (current : rest) = encodeModifiedHelper rest current 1

encodeModifiedHelper :: (Eq a) => [a] -> a -> Int -> [RLE a]
encodeModifiedHelper (current : rest) last count
  | rest == [] && current == last = [Multiple (count + 1) last]
  | rest == [] && current /= last && count /= 1 = [Multiple count last, Single current]
  | rest == [] && current /= last && count == 1 = [Single last, Single current]
  | current == last = encodeModifiedHelper rest current (count + 1)
  | current /= last && count /= 1 = [Multiple count last] ++ encodeModifiedHelper rest current 1
  | current /= last && count == 1 = [Single last] ++ encodeModifiedHelper rest current 1
