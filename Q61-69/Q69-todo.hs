-- Q69, write a function that goes from a tree to a preorder dot string

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show)

tree2ds :: (Tree Char) -> String
tree2ds Empty = "."
tree2ds (Branch val l r) = [val] ++ (tree2ds l) ++ (tree2ds r)

-- ds2tree :: String -> (Tree Char)
-- ds2tree [] = Empty
-- ds2tree ('.':rest) = Empty
-- ds2tree (current:rest) = let part = (partition rest 0 0) in (Branch current (ds2tree (part!!0)) (ds2tree (part!!1)))

-- partition :: String -> Int -> Int -> [String]
-- partition ds current 2 = [take (current - 1) ds,drop (current - 1) ds]
-- partition ds current count | current == (length ds) = [ds,[]]
--                            | ds!!current /= '.' = partition ds (current + 1) (count + 1)
--                            | otherwise = partition ds (current + 1) count