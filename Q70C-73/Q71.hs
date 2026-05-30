-- Q71.hs, find the internal path lenght of a multi-way tree

data Tree a = Node a [(Tree a)]

ipl :: (Tree a) -> Int
ipl (Node val []) = 0
ipl (Node val list) = foldr (+) ((length list) + 1) (map (ipl) list)