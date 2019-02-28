module USCanadaPhone where

import Text.Trifecta
import Control.Applicative

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber
    NumberingPlanArea
    Exchange
    LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone =
  PhoneNumber
    <$> (
      parens (read <$> count 3 digit) <|> do
      optional (try $ digit >> char '-')
      numberingPlanArea <- read <$> count 3 digit
      optional (char '-')
      return numberingPlanArea
    )
    <*> (read <$> count 3 digit)
    <*  optional (char '-')
    <*> (read <$> count 4 digit)
    <*  eof

test :: IO ()
test = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
