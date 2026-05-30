-- Q61, count the number of leaves in a binary tree

data Tree a = Empty | Branch a (Tree a) (Tree a)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch var Empty Empty) = 1
countLeaves (Branch var l r) = (countLeaves l) + (countLeaves r)