module WordNumber where

import Control.Exception
import Data.List

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

main :: IO ()
main =
  print $ assert (results == "one-two-three-two-four-five-four-six") results
  where results = wordNumber 12324546
