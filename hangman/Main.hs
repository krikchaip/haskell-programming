module Main where

import Hangman
import Data.Char (toLower)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  puzzle <- freshPuzzle . map toLower <$> randomWord
  runGame puzzle
