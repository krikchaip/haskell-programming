module SemVer where

-- Semantic Versioning
-- ref: https://semver.org

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Char (isDigit)

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
  NOSS String | NOSI Integer
  deriving (Show, Eq, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

noLeadingZero :: Parser Integer
noLeadingZero = do
  ds <- some digit
  if length ds > 1 && head ds == '0'
    then unexpected "contain leading zero"
    else return (read ds)

asciiAlphaNumHyphen :: Parser String
asciiAlphaNumHyphen =
  let set = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-"
  in  some $ oneOf set

identifier :: Parser NumberOrString
identifier =
      (NOSS <$> try (asciiAlphaNumHyphen >>= turnback))
  <|> (NOSI <$> noLeadingZero)
  where
    turnback :: String -> Parser String
    turnback str =
      if isDigit `all` str
        then unexpected "cursor back to begining"
        else return str

preRelease :: Parser Release
preRelease = char '-' >> identifier `sepBy1` char '.'

metadata :: Parser Metadata
metadata = do
  char '+'
  meta <- asciiAlphaNumHyphen `sepBy1` char '.'
  return $ NOSS <$> meta

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- noLeadingZero
  char '.'
  minor <- noLeadingZero
  char '.'
  patch <- noLeadingZero
  release <- option [] preRelease
  meta <- option [] metadata
  eof
  return $ SemVer major minor patch release meta

parse :: String -> IO ()
parse str = do
  putStrLn $ "========== " ++ str ++ " =========="
  print $ parseString parseSemVer mempty str
  putChar '\n'

test :: IO ()
test = do
  let successCases = do
        parse "1.2.3"
        parse "0.0.0"
        parse "1.2.3-a"
        parse "1.2.3-1"
        parse "1.2.3-a1"
        parse "1.2.3-alpha-1.9.-x-y-z.1"
        parse "1.2.3+a"
        parse "1.2.3+1"
        parse "1.2.3+1a"
        parse "1.2.3+a1"
        parse "1.2.3+alpha-1.9.-xyz.1"
        parse "1.0.0-alpha+001"
        parse "1.0.0+20130313144700"
        parse "1.0.0-beta+exp.sha.5114f85"
        parse "1.2.3-a+b"
        parse "1.2.3-1.alpha-2.9+2.beta-1.99"
  let failCases = do
        parse ".2.3"
        parse "1..3"
        parse "1.2."
        parse "1.."
        parse ".2."
        parse "..3"
        parse ".."
        parse "1.2"
        parse "1."
        parse "1"
        parse "a"
        parse "-"
        parse " "
        parse ""
        parse "a.0.0"
        parse "0.a.0"
        parse "0.0.a"
        parse "-1.1.1"
        parse "1.-1.1"
        parse "1.1.-1?"
        parse "1.2.3-"
        parse "1.2.3-."
        parse "1.2.3-a."
        parse "1.2.3-á"
        parse "1.2.3-1."
        parse "1.2.3-1.1."
        parse "1.2.3-1:"
        parse "1.2.3-1?"
        parse "1.2.3+"
        parse "1.2.3+."
        parse "1.2.3+a."
        parse "1.2.3+á"
        parse "1.2.3+1."
        parse "1.2.3+1.1."
        parse "1.2.3+1:"
        parse "1.2.3+1+"
        parse "1.2.3-a+"
        parse "1.2.3-.+a"
        parse "1.2.3-+a"
        parse "1.2.3-+a."

  -- parse "2.1.1"
  -- parse "1.0.0-x.7.z.92"
  -- print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
  successCases
