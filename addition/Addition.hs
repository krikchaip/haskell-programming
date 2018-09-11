module Addition where

import Test.Hspec
import Test.QuickCheck

-- trivial generator of values (Gen is also a Monad)
-- try "sample trivialInt" on Ghci
trivialInt :: Gen Int
trivialInt = return 1

-- create random element generator
-- try "sample' oneThroughThree" on Ghci
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

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

-----------------------------------------------------------
{- Using QuickCheck without Hspec -}

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionGreater' :: Int -> Bool
prop_additionGreater' x = x + 0 > x

runQc :: IO ()
runQc = do
  quickCheck prop_additionGreater  -- success case
  quickCheck prop_additionGreater' -- failure case
-----------------------------------------------------------

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
          go (n - d) d (count + 1)

mult :: (Integral a) => a -> a -> a
mult n 1 = n
mult n x = n + mult n (x - 1)

main :: IO ()
main = hspec $ do
  describe "Division" $ do
    it "15 divided by 3 is 5" $
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
      \ 4 remainder 2" $
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "Multiplication" $ do
    it "3 mult 3 is 9" $
      3 `mult` 3 `shouldBe` 9
    it "0 mult anything should be 0" $ do
      0 `mult` 1 `shouldBe` 0
      0 `mult` 2 `shouldBe` 0

  describe "Addition" $
    it "x + 1 is always\
      \ greater than x" $
      property $ \x -> (x :: Int) + 1 > x
