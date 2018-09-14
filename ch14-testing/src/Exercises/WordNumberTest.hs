module Exercises.WordNumberTest where

import Data.List
import Test.Hspec

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits 0 = []
digits n = digits next ++ [digit]
  where digit = n `mod` 10
        next  = n `div` 10

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

wordNumberTest :: IO ()
wordNumberTest = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
