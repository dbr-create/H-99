-- Q63, generate a complete binary tree with n nodes

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show)

completeBinaryTree :: Int -> (Tree Char)
completeBinaryTree max | max > 0 = completeBinaryHelper max 1
                       | otherwise = Empty

completeBinaryHelper :: Int -> Int -> (Tree Char)
completeBinaryHelper max current | current <= max = Branch 'x' (completeBinaryHelper max (current * 2)) (completeBinaryHelper max ((current * 2) + 1))
                                 | otherwise = Empty