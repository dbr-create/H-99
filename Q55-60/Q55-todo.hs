-- Q55, return all possible balanced tree permutations for a given number of nodes

data Tree a = Branch a (Tree a) (Tree a) | Empty
  deriving (Show)

cabalTree :: Int -> [Tree Char]
cabalTree 0 = [Empty]
cabalTree 1 = [Branch 'x' Empty Empty]
cabalTree 2 = [Branch 'x' (Branch 'x' Empty Empty) Empty, Branch 'x' Empty (Branch 'x' Empty Empty)]
cabalTree n = [Branch 'x' l r | l <- cabalTree (div (n) 2), r <- cabalTree (div (n) 2)]