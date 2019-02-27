module Ch24_Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction    = "1/0"
alsoBad        = "10"
shouldWork     = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denumerator <- decimal
  return (numerator % denumerator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- exercise: trytry
type TryTry = Either Integer Rational

trytry :: Parser (Either Integer Rational)
trytry =
      (Right <$> try virtuousFraction)
  <|> (Left  <$> decimal <* eof)

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

testTryTry = do
  let trytry' =
        parseString trytry mempty
  print $ trytry' badFraction
  print $ trytry' alsoBad
  print $ trytry' shouldWork
  print $ trytry' shouldAlsoWork
