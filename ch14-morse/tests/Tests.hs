module Main where

import Morse

import qualified Data.Map as M
import Test.QuickCheck

main :: IO ()
main =
  quickCheck $ withMaxSuccess 500 backAndForthConvert
  where backAndForthConvert :: Property
        backAndForthConvert =
          forAll allowedChars
                 (\c -> (charToMorse c >>= morseToChar) == Just c)

{-
  Because Char includes thousands of characters
  that have no legitimate equivalent in Morse code,
  we need to write our own custom generators.
-}

allowedChars :: Gen Char
allowedChars = elements $ M.keys letterToMorse

allowedMorse :: Gen Morse
allowedMorse = elements $ M.elems letterToMorse
