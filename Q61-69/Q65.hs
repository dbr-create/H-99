--Q65, figure out the layout scheme and write a function to generate it for a given binary tree

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show)

maxDepth :: (Tree a) -> Int
maxDepth Empty = 0
maxDepth (Branch val l r) = 1 + max (maxDepth l) (maxDepth r)

findLeft :: (Tree a) -> Int -> Int
findLeft Empty _ = 0
findLeft (Branch val l r) max = 2^max + findLeft l (max - 1)

layout :: (Tree a) -> Tree (a,(Int,Int))
layout tree = layoutHelper tree ((maxDepth tree)) 1 ((findLeft tree ((maxDepth tree) - 1)) + 1) True

layoutHelper :: (Tree a) -> Int -> Int -> Int -> Bool -> Tree (a,(Int,Int))
layoutHelper Empty _ _ _ _ = Empty
layoutHelper (Branch val l r) max current parent left | left == True = let x = (parent - 2^(max - (current))) in Branch (val,(x,current)) (layoutHelper l max (current + 1) x True) (layoutHelper r max (current + 1) x False)
                                                      | otherwise = let x = (parent + 2^(max - (current))) in Branch (val,(x,current)) (layoutHelper l max (current + 1) x True) (layoutHelper r max (current + 1) x False)
