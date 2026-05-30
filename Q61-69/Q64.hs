
data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show)

findNodes :: (Tree a) -> Int
findNodes Empty = 0
findNodes (Branch val l r) = 1 + (findNodes l) + (findNodes r) 

layout :: (Tree a) -> (Tree (a,(Int,Int)))
layout tree = layoutHelper tree ((findNodes tree) + 1) 1 True


layoutHelper :: (Tree a) -> Int -> Int -> Bool -> (Tree (a,(Int,Int)))
layoutHelper Empty _ _ _ = Empty
layoutHelper (Branch val l r) parent depth boolLeft = let new_parent = (if boolLeft == True then (parent - 1 - (findNodes r)) else (1 + parent + findNodes l)) in Branch (val,(new_parent,depth)) (layoutHelper l new_parent (depth + 1) True) (layoutHelper r new_parent (depth + 1) False) 