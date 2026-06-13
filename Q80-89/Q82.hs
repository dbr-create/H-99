-- Q82, write a predicate to find all cycles in a graph

cycleMine :: (Eq a) => a -> [(a,a)] ->  [[a]]
cycleMine node1 arcs = map ([node1] ++) (cycleHelper True node1 node1 arcs [])

cycleHelper :: (Eq a) => Bool -> a -> a -> [(a,a)] -> [a] -> [[a]]
cycleHelper start node1 node2 arcs accumulator | (node1 == node2) && (start == False) = [accumulator]
                                               | arcs == [] = []
                                               | otherwise = concat [cycleHelper False u node2 ((take pos arcs) ++ (drop (pos + 1) arcs))(accumulator ++ [u]) |((s,u),pos) <- (zip arcs [0..]), (node1 == s)] 