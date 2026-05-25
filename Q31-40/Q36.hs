--Q36, find all prime factors and their multiplicity

multiplicity :: Int -> [(Int,Int)] 
multiplicity num = multiplicityHelper 1 1 (primeFactors num)

multiplicityHelper :: Int -> Int -> [Int] -> [(Int,Int)]
multiplicityHelper count last [] = [(last,count)]
multiplicityHelper _ 1 (value:rest) = multiplicityHelper 1 (value) (rest)
multiplicityHelper count last (value:rest) | value == last = multiplicityHelper (count + 1) last (rest)
                                           | otherwise = [(last,count)] ++ multiplicityHelper 1 value (rest)

primeFactors :: Int -> [Int]
primeFactors num | num > 1 = primeFactorsHelper 2 num
                 | otherwise = []

primeFactorsHelper :: Int -> Int -> [Int]
primeFactorsHelper currVal num | currVal == num = [currVal]
                               | mod num currVal == 0 = [div num (div num currVal)] ++ primeFactorsHelper currVal (div num currVal)
                               | otherwise = primeFactorsHelper (currVal + 1) num