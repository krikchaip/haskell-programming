module Ch09_Cipher where

import Data.Char
import Data.Int

digitRange :: [Int]
digitRange = [48..57]

upperRange :: [Int]
upperRange = [65..90]

lowerRange :: [Int]
lowerRange = [97..122]

cipher :: Int8 -> Char -> Char
cipher n c
  | isDigit      c = shiftWithIn digitRange
  | isAsciiUpper c = shiftWithIn upperRange
  | isAsciiLower c = shiftWithIn lowerRange
  | otherwise      = c
  where
    shiftWithIn range = chr . clamp range . (+fromIntegral n) . ord $ c
    clamp range x
      | x > maximum range = minimum range + (x - maximum range) - 1
      | x < minimum range = maximum range - (minimum range - x) + 1
      | otherwise         = x

caesar :: String -> String
caesar = map $ cipher (-3)

unCaesar :: String -> String
unCaesar = map $ cipher 3
