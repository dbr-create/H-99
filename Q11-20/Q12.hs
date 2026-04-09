-- Q12, decoding RLE from previous question
data RLE a = Single a | Multiple Int a
  deriving (Show)

decodeModified :: [RLE a] -> [a]
decodeModified [] = []
decodeModified (Single current : rest) = [current] ++ decodeModified rest
decodeModified (Multiple count current : rest) = replicate count current ++ decodeModified rest
