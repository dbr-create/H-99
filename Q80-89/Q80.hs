-- Q80, Create predicates to transfer between the three graph forms

-- forms

data Arc a = Arc a a
    deriving (Show)

data AdjacencyList a = Adj [(a, [a])]
    deriving (Show)

data Digraph a = Dig ([a],[Arc a])
    deriving (Show)

-- Digraph <-> [Arc]

dijToArc :: Digraph a -> [Arc a]
dijToArc (Dig (nodeList,arcList)) = arcList

arcToDij :: (Eq a) => [Arc a] -> Digraph a
arcToDij arcList = Dig (unique [] arcList,arcList)

unique :: (Eq a) => [a] -> [Arc a] -> [a]
unique accumulator [] = accumulator
unique accumulator ((Arc val1 val2):restArc) = let val1In = (if (foldr (||) (False) (map (==val1) accumulator)) == True then [] else [val1]); val2In = (if (foldr (||) (False) (map (==val2) accumulator)) == True then [] else [val2]) in unique (accumulator ++ val1In ++ val2In) (restArc)

-- [Arc] <-> Adj

arcToAdj :: (Eq a) => [Arc a] -> AdjacencyList a
arcToAdj arcs = Adj (arcToAdjHelper arcs []) 

arcToAdjHelper :: (Eq a) => [Arc a] -> [(a,[a])] -> [(a,[a])]
arcToAdjHelper [] accumulator = accumulator
arcToAdjHelper ((Arc val1 val2):rest) accumulator = let position = (checkAccumulatorPos val1 0 accumulator) in (if position == -1 then arcToAdjHelper (rest) (accumulator ++ [(val1,[val2])]) else arcToAdjHelper (rest) ((take position accumulator) ++ (let (var,list) = (accumulator!!position) in [(var,list ++ [val2])]) ++ (drop (position + 1) accumulator)))

checkAccumulatorPos :: (Eq a) => a -> Int -> [(a,[a])] -> Int
checkAccumulatorPos _ _ [] = -1
checkAccumulatorPos val1 count ((val2,list):rest) | val1 == val2 = count
                                                  | otherwise = checkAccumulatorPos val1 (count + 1) (rest)
