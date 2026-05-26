-- Q40, find the two primes that add to a number - goldbacks conjecture

goldback :: Integer -> (Integer,Integer)
goldback num = goldbackHelper 2 num [] (primeR 3 num)

goldbackHelper :: Integer -> Integer -> [Integer] -> [Integer] -> (Integer,Integer)
goldbackHelper current goal accumulator [] = goldbackHelper (accumulator!!0) goal [] (drop 1 accumulator)
goldbackHelper current goal accumulator (value:rest) | current + value == goal = (current,value)
                                                     | otherwise = goldbackHelper current goal (accumulator ++ [value]) (rest)

primeR :: Integer -> Integer -> [Integer]
primeR num1 num2 | num1 == num2 = []
                 | isPrime num1 == True = [num1] ++ primeR (num1 + 1) num2
                 | otherwise = primeR (num1 + 1) num2

isPrime :: Integer -> Bool
isPrime num | num < 2 = False
            | otherwise = isPrimeHelper 2 num

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper curr num | (curr > (div num 2) + 1) = True
                       | (curr < (div num 2) + 1) && mod num curr == 0 = False
                       | otherwise = isPrimeHelper (curr + 1) num