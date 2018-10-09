module Exercises.Instances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "\n##### Pair #####"
  quickBatch $ applicative (undefined :: Pair (Int, Int, Int))

  putStrLn "\n##### Two #####"
  quickBatch $ applicative (undefined :: Two String (Int, Int, Int))

-- ########## Pair ########## -------------------------------------------------
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- ########## Two ########## --------------------------------------------------
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a f) <*> (Two a' x) = Two (a <> a') (f x)

instance (Monoid a, Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq
