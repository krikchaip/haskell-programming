module Ch08_Recursion where

incTimes :: (Eq t, Num t, Num t1) => t -> t1 -> t1
incTimes 0     n = n
incTimes times n = 1 + incTimes (times - 1) n

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

simpleSeries :: (Eq a, Num a) => a -> a
simpleSeries 1 = 1
simpleSeries n = n + simpleSeries (n - 1)

mult :: (Integral a) => a -> a -> a
mult n 1 = n
mult n x = n + mult n (x - 1)

data DividedResult = Result Integer | DividedByZero deriving Show

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy x n = Result (x `div` n)

mc91 :: Integral t => t -> t
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 $ mc91 (n + 11)
