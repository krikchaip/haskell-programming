module Arbitrary.Simple where

import Test.QuickCheck

-- or "sample (arbitrary :: Gen (a, b))"
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- or "sample (arbitrary :: Gen (a, b, c))"
genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

-- or "sample (arbitrary :: Gen (Either a b))"
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
-- or "sample (arbitrary :: Gen (Maybe a))"
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
