module Ch20_LibraryFunctions where

-- import Data.Foldable
import Data.Monoid
-- import Data.Semigroup

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\a -> Any $ x == a)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr compM Nothing
  where compM x Nothing  = Just x
        compM x (Just y) = if x <= y then Just x else Just y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr compM Nothing
  where compM x Nothing  = Just x
        compM x (Just y) = if x >= y then Just x else Just y

null' :: (Foldable t) => t a -> Bool
null' = (<1) . length

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty
