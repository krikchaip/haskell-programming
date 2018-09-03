{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch07_Exercises where

-- * ex01 **********
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' = (`mod` 10) . fst . (`divMod` 10)

hunsD' :: Integral a => a -> a
hunsD' = (`mod` 10) . fst . (`divMod` 10) . fst . (`divMod` 10)

-- * ex02 **********
foldBool :: p -> p -> Bool -> p
foldBool x y z = if z then x else y

foldBoolMatch :: p -> p -> Bool -> p
foldBoolMatch x _ False = x
foldBoolMatch _ y True = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y z =
  case z of
    True  -> x
    False -> y

foldBoolGuard :: p -> p -> Bool -> p
foldBoolGuard x y z
  | z = x
  | otherwise = y

-- * ex03 **********
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- * ex04 **********
arith4 :: IO ()
arith4 = do
  print (roundTrip 4)
  print (id 4)
  ex06
  where

-- * ex05 **********
    roundTrip :: (Show a, Read a) => a -> a
    roundTrip = read . show

-- * ex06 **********
    roundTrip' :: (Show a, Read b) => a -> b
    roundTrip' = read . show

    ex06 = print (roundTrip' 4 :: Integer)
