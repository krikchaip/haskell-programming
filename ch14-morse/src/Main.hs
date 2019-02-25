module Main where

import Morse (stringToMorse, morseToChar)

import Data.Traversable (sequence)

import Control.Monad (forever, when)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to"   -> convertToMorse
        _      -> argError
    _     -> argError
  where argError = do
          putStrLn "Please specify the\
                  \ first argument\
                  \ as being 'from' or\
                  \ 'to' morse,\
                  \ such as: '$ morse to'"
          exitFailure



convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess

  -- otherwise, proceed.
  line <- getLine
  convertLine line

  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        Just str -> putStrLn (unwords str)
        Nothing  -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess

  -- otherwise, proceed.
  line <- getLine
  convertLine line

  where
    convertLine line = do
      let decoded = sequence $ fmap morseToChar (words line)
      case decoded of
        Just str -> putStrLn str
        Nothing  -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure
