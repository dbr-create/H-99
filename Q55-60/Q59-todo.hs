-- Q58, generate a list of all height balanced subtrees with a max depth n (for a given char)

data Tree a = Branch a (Tree a) (Tree a) | Empty
  deriving (Show)

hbalTree :: Char -> Int -> [Tree Char]
hbalTree _ 0 = [Empty]
hbalTree char 1 = [Branch char Empty Empty]
hbalTree char 2 = [Branch char (Branch char Empty Empty) Empty, Branch char Empty (Branch char Empty Empty)]
hbalTree char n = [Branch char Empty Empty] ++ [Branch char l r | l <- hbalTree char (div n 2), r <- hbalTree char (div n 2)]
