-- Q61A, collect all leaves in a tree into a list

data Tree a = Empty | Branch a (Tree a) (Tree a)

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch var Empty Empty) = [var]
leaves (Branch var l r) = (leaves l) ++ (leaves r)