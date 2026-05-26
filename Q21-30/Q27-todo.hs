-- group :: [int] -> [a] -> [[[a]]]
-- group nums values | foldl (+) 0 nums /= length values = []
--                   | otherwise = groupHelper [] nums values

-- groupHelper :: [[[a]]] -> [int] -> [a] -> [[[a]]]
-- groupHelper accumulator nums [] = accumulator
-- groupHelper accumulator nums values = undefined


groupGenerator :: Eq a => [a] -> Integer -> [a] -> [[a]] 
groupGenerator accumulator n values | values == [] && (n /= 0) = []
                                    | n == 0 = [accumulator]
                                    | otherwise = concat [groupGenerator (accumulator ++ [currVal]) (n-1) (drop (pos + 1) values) | (currVal,pos) <- zip values [0..]]