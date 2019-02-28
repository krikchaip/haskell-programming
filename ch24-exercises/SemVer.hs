module SemVer where

-- Semantic Versioning
-- ref: https://semver.org

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Char (isDigit)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
  NOSS String | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  (NOSS s) `compare` (NOSS s') = compare s s'
  (NOSI i) `compare` (NOSI i') = compare i i'
  (NOSI _) `compare` (NOSS _)  = LT
  (NOSS _) `compare` (NOSI _)  = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare
    (SemVer maj  min  pat  rel  _)
    (SemVer maj' min' pat' rel' _)
    | maj /= maj' = compare maj maj'
    | min /= min' = compare min min'
    | pat /= pat' = compare pat pat'
    | all null [rel, rel'] = EQ
    | null rel             = GT
    | null rel'            = LT
    | otherwise  = compare rel rel'

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
parseSemVer =
  SemVer <$> noLeadingZero <* char '.'
         <*> noLeadingZero <* char '.'
         <*> noLeadingZero
         <*> option [] preRelease
         <*> option [] metadata
         <*  eof

parse :: String -> Result SemVer
parse = parseString parseSemVer mempty

parsePrint :: String -> IO ()
parsePrint str = do
  putStrLn $ "========== " ++ str ++ " =========="
  print $ parse str
  putChar '\n'

parseOrd :: String -> Maybe SemVer
parseOrd = maybeSuccess . parse

test :: IO ()
test = do
  let successCases = do
        parsePrint "1.2.3"
        parsePrint "0.0.0"
        parsePrint "1.2.3-a"
        parsePrint "1.2.3-1"
        parsePrint "1.2.3-a1"
        parsePrint "1.2.3-alpha-1.9.-x-y-z.1"
        parsePrint "1.2.3+a"
        parsePrint "1.2.3+1"
        parsePrint "1.2.3+1a"
        parsePrint "1.2.3+a1"
        parsePrint "1.2.3+alpha-1.9.-xyz.1"
        parsePrint "1.0.0-alpha+001"
        parsePrint "1.0.0+20130313144700"
        parsePrint "1.0.0-beta+exp.sha.5114f85"
        parsePrint "1.2.3-a+b"
        parsePrint "1.2.3-1.alpha-2.9+2.beta-1.99"
  let failCases = do
        parsePrint ".2.3"
        parsePrint "1..3"
        parsePrint "1.2."
        parsePrint "1.."
        parsePrint ".2."
        parsePrint "..3"
        parsePrint ".."
        parsePrint "1.2"
        parsePrint "1."
        parsePrint "1"
        parsePrint "a"
        parsePrint "-"
        parsePrint " "
        parsePrint ""
        parsePrint "a.0.0"
        parsePrint "0.a.0"
        parsePrint "0.0.a"
        parsePrint "-1.1.1"
        parsePrint "1.-1.1"
        parsePrint "1.1.-1?"
        parsePrint "1.2.3-"
        parsePrint "1.2.3-."
        parsePrint "1.2.3-a."
        parsePrint "1.2.3-á"
        parsePrint "1.2.3-1."
        parsePrint "1.2.3-1.1."
        parsePrint "1.2.3-1:"
        parsePrint "1.2.3-1?"
        parsePrint "1.2.3+"
        parsePrint "1.2.3+."
        parsePrint "1.2.3+a."
        parsePrint "1.2.3+á"
        parsePrint "1.2.3+1."
        parsePrint "1.2.3+1.1."
        parsePrint "1.2.3+1:"
        parsePrint "1.2.3+1+"
        parsePrint "1.2.3-a+"
        parsePrint "1.2.3-.+a"
        parsePrint "1.2.3-+a"
        parsePrint "1.2.3-+a."
  let metaCases = do
        print $ parseOrd "1.0.0-alpha" < parseOrd "1.0.0-alpha.1"
        print $ parseOrd "1.0.0-alpha.1" < parseOrd "1.0.0-alpha.beta"
        print $ parseOrd "1.0.0-alpha.beta" < parseOrd "1.0.0-beta"
        print $ parseOrd "1.0.0-beta" < parseOrd "1.0.0-beta.2"
        print $ parseOrd "1.0.0-beta.2" < parseOrd "1.0.0-beta.11"
        print $ parseOrd "1.0.0-beta.11" < parseOrd "1.0.0-rc.1"
        print $ parseOrd "1.0.0-rc.1" < parseOrd "1.0.0"

  -- parse "2.1.1"
  -- parse "1.0.0-x.7.z.92"
  -- print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []

  successCases
  failCases
  metaCases
