module Ch10_Fibs where

-- fibs
-- = 1 : scanl + 1 fibs <- fibs ~ (1 : ...)
-- = 1 : (1 : scanl + 2 fibs) <- fibs ~ (1 : ...)
-- = 1 : (1 : (2 : scanl + 3 fibs)) <- fibs ~ (2 : ...)
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN n = fibs !! n

first20Fibs :: [Integer]
first20Fibs = take 20 fibs

lessThan100Fibs :: [Integer]
lessThan100Fibs = takeWhile (<100) fibs

fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact n = n * fact (n-1)

-- facts
-- = scanl * 1 [1..]
-- = 1 : scanl * (1*1) [2..]
-- = 1 : 1 : scanl * (1*2) [3..]
-- = 1 : 1 : 2 : scanl * (2*3) [4..]
-- = 1 : 1 : 2 : 6 : scanl * (6*4) [5..]
facts :: [Integer]
facts = scanl (*) 1 [1..]
