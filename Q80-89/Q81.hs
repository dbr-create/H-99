-- Q81, find all acyclic paths in a graph

paths :: (Eq a) => a -> a -> [(a,a)] ->  [[a]]
paths node1 node2 arcs = map ([node1] ++) (pathsHelper node1 node2 arcs [])

pathsHelper :: (Eq a) => a -> a -> [(a,a)] -> [a] -> [[a]]
pathsHelper node1 node2 arcs accumulator | node1 == node2 = [accumulator]
                                         | arcs == [] = []
                                         | otherwise = concat [pathsHelper u node2 ((take pos arcs) ++ (drop (pos + 1) arcs))(accumulator ++ [u]) |((s,u),pos) <- (zip arcs [0..]), (node1 == s)] 