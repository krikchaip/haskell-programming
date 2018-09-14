module Hangman where

import Control.Monad (forever, when)

import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)

import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

type ExactWord  = String
type Discovered = [Maybe Char]
type Guessed    = [Char]

data Puzzle = Puzzle ExactWord Discovered Guessed

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: "
    ++ guessed

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = lines <$> readFile "data/dict.txt"

gameWords :: IO WordList
gameWords = filter gameLength <$> allWords
  where gameLength w = minWordLength <= length w && length w < maxWordLength

randomWord :: IO String
randomWord = gameWords >>= randIndex
  where randIndex xs = do
          range <- randomRIO (0, length xs - 1)
          return (xs !! range)

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s nothings []
  where nothings = map (const Nothing) s

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) = (`elem` s)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) = (`elem` g)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle w ds gs) c = Puzzle w ds' gs'
  where ds' = zipWith (justify c) w ds
        gs' = if c `elem` w then gs else c : gs
        justify c wchar dschar = if c == wchar then Just c else dschar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (puzzle `charInWord` guess
      , puzzle `alreadyGuessed` guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return $ puzzle `fillInCharacter` guess
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return $ puzzle `fillInCharacter` guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word discovered guessed) =
  when (length guessed > length word) $ do
    if discovered `areAll` justs
      then putStrLn "You win!"
      else do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ word
    exitSuccess
  where areAll = flip all
        justs  = isJust

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _) =
  when (discovered `areAll` justs) $ do
    putStrLn "You win!"
    exitSuccess
  where areAll = flip all
        justs  = isJust

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                   \ be a single character"

