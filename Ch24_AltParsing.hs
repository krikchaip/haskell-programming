{-# LANGUAGE QuasiQuotes #-}

module Ch24_AltParsing where

import Text.Trifecta
import Text.RawString.QQ
import Control.Applicative

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

(<<) :: Monad m => m a -> m b -> m a
ma << mb = do
  a <- ma
  _ <- mb
  return a

-- Quasiquotation string (aka. template literal)
eitherOr :: String
eitherOr = [r|
  123
  abc
  456
  def
|]

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

parseNos' :: Parser [NumberOrString]
parseNos' = many $
  skipMany (oneOf "\n ")  >>
  (Left  <$> integer)     <|>
  (Right <$> some letter) <<
  skipMany (oneOf "\n ")

main :: IO ()
main = do
  print $ parseString (some letter)   mempty a
  print $ parseString integer         mempty b
  print $ parseString parseNos        mempty a
  print $ parseString parseNos        mempty b
  print $ parseString (many parseNos) mempty c
  print $ parseString (some parseNos) mempty c

  -- with quasi string
  print $ parseString parseNos  mempty eitherOr
  print $ parseString parseNos' mempty eitherOr
