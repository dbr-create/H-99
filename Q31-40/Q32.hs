-- Q31, give the gcd of two numbers

myGcd :: Int -> Int -> Int
myGcd 0 _ = 0
myGcd _ 0 = 0
myGcd num1 num2 | (negateHelper num1) >= (negateHelper num2) = gcdHelper (negateHelper num1) (negateHelper num2)
                | otherwise = gcdHelper (negateHelper num2) (negateHelper num1)


negateHelper :: Int -> Int
negateHelper num | num < 0 = (-num)
                 | num > 0 = num

gcdHelper :: Int -> Int -> Int
gcdHelper num1 num2 | mod num1 num2 == 0 = div num1 (div num1 num2)
                    | otherwise = gcdHelper (num1 - (mod num1 num2)) (mod num1 num2)