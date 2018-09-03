module Ch09_Exercises where

import Data.Char

capitalize :: [Char] -> [Char]
capitalize (x:xs) = toUpper x : xs

upper :: [Char] -> [Char]
upper []     = []
upper (x:xs) = toUpper x : upper xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem a (x:xs) = a == x || myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []       = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x1:x2:xs) =
  if f x1 x2 == GT
  then myMaximumBy f (x1:xs)
  else myMaximumBy f (x2:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x1:x2:xs) =
  if f x1 x2 == LT
  then myMinimumBy f (x1:xs)
  else myMinimumBy f (x2:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
