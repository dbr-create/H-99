-- Q57, write a predicate to create a binary search tree from a list of integers

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show)

construct :: [Int] -> Tree Int
construct [] = Empty
construct (current : rest) = constructHelper (Branch current Empty Empty) (rest)

constructHelper :: Tree Int -> [Int] -> Tree Int
constructHelper accumulator [] = accumulator
constructHelper accumulator (current : rest) = constructHelper (insert accumulator current) (rest)

insert :: Tree Int -> Int -> Tree Int
insert Empty current = Branch current Empty Empty
insert (Branch val l r) current
  | current < val = Branch val (insert l current) r
  | otherwise = Branch val l (insert r current)
