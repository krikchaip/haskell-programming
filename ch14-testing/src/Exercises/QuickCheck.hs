module Exercises.QuickCheck where

-- import Test.QuickCheck
import Data.Char
import Data.List (sort)

-- ex01
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

ex01_qc :: (Eq a, Fractional a) => a -> Bool
ex01_qc x = halfIdentity x == x

-- ex02
-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

ex02_qc :: (Ord a) => [a] -> Bool
ex02_qc = listOrdered . sort

-- ex03
{- this properties won’t hold for types
   based on IEEE-754 floating point numbers,
   such as Float or Double. -}
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
-- plusAssociative :: Float -> Float -> Float -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

-- ex04
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

-- ex05
quot_rem :: Integral a => a -> a -> Bool
quot_rem x y = y /= 0 && (quot x y) * y + (rem x y) == x

div_mod :: Integral a => a -> a -> Bool
div_mod x y =  y /= 0 && (div x y) * y + (mod x y) == x

-- ex06 (check whether exponent operator has these following properties)
expAssociative :: (Integral b, Integral b1, Num a, Eq a) => a -> b -> b1 -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

expCommutative :: Integral b => b -> b -> Bool
expCommutative x y = x ^ y == y ^ x

-- ex07
reverseId :: Eq a => [a] -> Bool
reverseId xs = reverse (reverse xs) == id xs

-- ex08 - (should run with some functions)
-- ex. quickCheck $ apply (+1)
apply :: Eq a => (t -> a) -> t -> Bool
apply f x = (f $ x) == f x

-- ex. quickCheck $ compose (*10) (+3)
compose :: Eq a => (b -> a) -> (a1 -> b) -> a1 -> Bool
compose f g x = (f . g $ x) == f (g x)

-- ex09
ex09_qc :: Eq a => [a] -> [a] -> Bool
ex09_qc xs ys = foldr (:) xs ys == xs ++ ys

ex09_qc' :: (Eq a, Foldable t) => t [a] -> Bool
ex09_qc' xss = foldr (++) [] xss == concat xss

-- ex10
ex10_qc :: Int -> [a] -> Bool
ex10_qc n xs = length (take n xs) == n

-- ex11
readShowRoundtrip :: (Read a, Show a, Eq a) => a -> Bool
readShowRoundtrip x = (read . show $ x) == x

{- Failure exercises -}
-- for a function
square :: Num a => a -> a
square x = x * x

-- why does this property not hold? -- Examine the type of sqrt.
-- ans: floating point precision change sparingly
-- during transition from(to) sqrt
squareIdentity :: (Eq a, Floating a) => a -> Bool
squareIdentity x = (square . sqrt $ x) == x

{- Idempotence exercises -}
twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

idem1 :: String -> Bool
idem1 x =
     (capitalizeWord x == twice capitalizeWord x)
  && (capitalizeWord x == fourTimes capitalizeWord x)
  where capitalizeWord ""     = ""
        capitalizeWord (c:cs) = toUpper c : cs

idem2 :: Ord a => [a] -> Bool
idem2 xs =
     (sort xs == twice sort xs)
  && (sort xs == fourTimes sort xs)
