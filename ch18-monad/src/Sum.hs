module Sum where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f sx = case sx of
    (Second b) -> Second $ f b
    (First a)  -> First a

instance Applicative (Sum a) where
  pure = Second
  (<*>) sf sx = case (sf, sx) of
    (First f, _)         -> First f
    (_, First x)         -> First x
    (Second f, Second x) -> Second $ f x

instance Monad (Sum a) where
  return = pure
  (>>=) sx f = case sx of
    (First x)  -> First x
    (Second x) -> f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

test :: IO ()
test = do
  quickBatch $ functor (undefined :: Sum Int (Int, Int, Int))
  quickBatch $ applicative (undefined :: Sum Int (Int, Int, Int))
  quickBatch $ monad (undefined :: Sum Int (Int, Int, Int))
