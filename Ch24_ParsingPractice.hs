module Ch24_ParsingPractice where

import           Text.Trifecta
import           Control.Applicative
import qualified Ch24_LearnParsers as Learn

parse :: Parser Char -> String -> IO ()
parse parser str =
  print (parseString parser mempty str)

block :: String -> IO a -> IO ()
block msg logs = do
  putStrLn $ "========== " ++ msg ++ " =========="
  logs
  putStrLn ""

-- ex01
ex01_one    = Learn.one    >> eof >> return '1'
ex01_oneTwo = Learn.oneTwo >> eof >> return '1'

-- ex02
ex02 =
  string' "123" <|>
  string' "12"  <|>
  string' "1"   >>
  Learn.stop

-- ex03
string' :: String -> Parser String
string' []     = return mempty
string' (x:xs) = do
  c <- char x
  (c :) <$> string' xs

test :: IO ()
test = do
  block "ex01 test" $ do
    ex01_one    `parse` "1"
    ex01_oneTwo `parse` "12"
    ex01_one    `parse` "12"
    ex01_oneTwo `parse` "123"
  block "ex02 test" $ do
    ex02 `parse` "1"
    ex02 `parse` "12"
    ex02 `parse` "123"
    ex02 `parse` "364"
  block "ex03 test" $ do
    (string' "win" >> Learn.stop) `parse` "w"
    (string' "win" >> Learn.stop) `parse` "wi"
    (string' "win" >> Learn.stop) `parse` "win"
