module Main where

import Test.Hspec
import Test.QuickCheck

import Exercises
import Generator
import Arbitrary
import CoArbitrary
import Property

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
