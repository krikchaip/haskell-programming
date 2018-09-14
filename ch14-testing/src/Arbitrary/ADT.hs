module Arbitrary.ADT where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

import Control.Applicative (liftA2)

{- Arbitrary Trivial -} -----------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = genTrivial

-- or "sample (arbitrary :: Gen Trivial)"
-- because we've already defined Arbitrary instance (arbitrary)
-- sample' genTrivial == [Trivial, Trivial, ...]
genTrivial :: Gen Trivial
genTrivial = return Trivial

{- Arbitrary Identity -} ----------------------------------
data Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

-- or "sample (genIdentity :: Gen (Identity ...))"
genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = Identity <$> arbitrary

genIdentityInt :: Gen (Identity Int)
genIdentityInt = genIdentity

{- Arbitrary Products -} ----------------------------------
data Pair a b = Pair a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

genPairIntString :: Gen (Pair Int String)
genPairIntString = arbitrary

{- Arbitrary Sum type -} ----------------------------------
data Sum a b = First a | Second b deriving (Eq, Show)

-- or write arbitrary instance...
-- equal chances for each
-- (the odds aren't affected by item frequencies)
genSumEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genSumEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

genSumCharInt :: Gen (Sum Char Int)
genSumCharInt = genSumEqual

-- 10 times higher probability to our First data constructor
genSumFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genSumFirstPls = frequency [(10, fmap First arbitrary)
                           ,(1, fmap Second arbitrary)]

genSumCharIntFirst :: Gen (Sum Char Int)
genSumCharIntFirst = genSumFirstPls
