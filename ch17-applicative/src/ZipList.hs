module ZipList where

import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipL a = ZipL [a] deriving (Eq, Show)

instance Semigroup a => Semigroup (ZipL a) where
  (ZipL xs) <> (ZipL ys) = ZipL $ zipWith (<>) xs ys

instance Monoid a => Monoid (ZipL a) where
  -- it's a `Zero` not an Identity.
  -- the result is always [] whenever this zips with something
  -- mempty = ZipL []
  mempty = ZipL $ repeat mempty

instance Functor ZipL where
  fmap f (ZipL xs) = ZipL $ f <$> xs -- just another representation for lists

instance Applicative ZipL where
  pure x = ZipL $ repeat x
  (ZipL fs) <*> (ZipL xs) = ZipL $ zipWith ($) fs xs

instance Arbitrary a => Arbitrary (ZipL a) where
  arbitrary = ZipL <$> arbitrary

-- just in case when we're testing infinite list
instance Eq a => EqProp (ZipL a) where
  (ZipL xs) =-= (ZipL ys) =
    let takeAll = take 3000
    in  takeAll xs `eq` takeAll ys

main :: IO ()
main = do
  quickBatch $ monoid (undefined :: ZipL String)
  quickBatch $ applicative (undefined :: ZipL (Integer, Char, String))
