-- Q70C, find the number of nodes in a muliway tree

data Tree a = Node a [(Tree a)]
    deriving (Eq, Show)

nnodes :: (Tree a) -> Int
nnodes (Node val []) = 1
nnodes (Node val list) = foldr (+) 1 (map (nnodes) list)