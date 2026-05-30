-- Q68a, Write a preorder and inorder predicate for a bianry tree

data (Tree a) = Empty | Branch a (Tree a) (Tree a)
    deriving (Show)

treeToInorder :: (Tree a) -> [a]
treeToInorder Empty = []
treeToInorder (Branch val l r) = (treeToInorder l) ++ [val] ++ (treeToInorder r)

treeToPreorder :: (Tree a) -> [a]
treeToPreorder Empty = []
treeToPreorder (Branch val l r) = [val] ++ (treeToPreorder l) ++ (treeToPreorder r)

--Q68b, Go from a preorder sequence to a tree 
-- (not nessisarily the same tree that produced the given tree just one that produces the same preorder sequence )

preorderToTree :: [a] -> (Tree a)
preorderToTree [] = Empty
preorderToTree (current:rest) = (Branch current Empty (preorderToTree rest))

--Q68c, given a preorder and an inorder traversal give the corresponding tree

preInTree :: (Eq a) => [a] -> [a] -> (Tree a)
preInTree preOrder [] = Empty
preInTree preOrder inOrder = let parent = (findParent preOrder inOrder); part = (findPartition parent 0 inOrder) in (Branch parent (preInTree preOrder (part!!0)) (preInTree preOrder (part!!1)))

findPartition :: (Eq a) => a -> Int -> [a] -> [[a]]
findPartition parent count inOrder | inOrder!!count == parent = [(take count inOrder),(drop (count + 1) inOrder)]
                                   | otherwise = findPartition parent (count + 1) inOrder

findParent :: (Eq a) => [a] -> [a] -> a
findParent (preCurrent:preRest) inOrder | (foldr (||) (False) (map (preCurrent==) inOrder) == True) = preCurrent
                                        | otherwise = findParent (preRest) inOrder