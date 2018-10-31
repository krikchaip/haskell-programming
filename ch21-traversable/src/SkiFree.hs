module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

-- instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
--   arbitrary = S <$> arbitrary <*> arbitrary

-- instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
--   (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> (pure <$> arbitrary) <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

skifree = do
  sample' (arbitrary :: Gen (S [] Int))
  quickBatch $ functor (undefined :: S [] (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: S [] (Int, Int, [Int]))
