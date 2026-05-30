-- Q56, write a predicate to check if a given binary tree is symmetric

data Tree a = Empty | Branch a (Tree a) (Tree a)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch val l r) = symmetricHelper l r

symmetricHelper :: Tree a -> Tree a -> Bool
symmetricHelper (Branch val l r) Empty = False
symmetricHelper Empty (Branch val l r) = False
symmetricHelper Empty Empty = True
symmetricHelper (Branch val1 l1 r1) (Branch val2 l2 r2) = (symmetricHelper l1 r2) && (symmetricHelper l2 r1)
