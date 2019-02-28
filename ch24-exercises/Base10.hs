module Base10 where

import Text.Trifecta
import Control.Applicative ((<|>))

parseDigit :: Parser Char
parseDigit = satisfyRange '0' '9'

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- option "" (string "-")
  read . (sign ++) <$> some parseDigit

test :: IO ()
test = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  print $ parseString base10Integer' mempty "-123abc"
