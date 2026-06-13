-- Q83, construct all possible spanning trees

spanningTree :: [(a,a)] -> [(a,a)]
spanningTree arcs = sTH arcs []

sTH :: [(a,a)] -> [a] -> [(a,a)]
sTH arcs accumulator