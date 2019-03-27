{-# LANGUAGE QuasiQuotes #-}

module LogFile where

import Text.Trifecta
import Text.RawString.QQ

import Control.Monad       (replicateM, void)
import Control.Applicative ((<|>))

import Data.List           (groupBy, sortOn)

example :: String
example = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

test :: IO ()
test = do
  let testComment = print . parseString comment  mempty
      testLog     = print . parseString parseLog mempty
      result =
        case parseString parseLog mempty example of
          Success xs -> xs
          Failure _  -> []

  -- testComment "-- wheee a comment"

  -- testLog [r|
  --   # 2025-02-05
  -- |]

  -- testLog [r|
  --   -- wheee a comment
  --   # 2025-02-05
  -- |]

  -- testLog [r|
  --   -- wheee a comment
  --   # 2025-02-07 -- dates not nececessarily sequential
  -- |]

  -- testLog [r|
  --   -- wheee a comment
  --   # 2025-02-05
  --   08:00 Breakfast
  -- |]

  -- testLog [r|
  --   # 2025-02-07 -- dates not nececessarily sequential
  --   08:00 Breakfast -- should I try skippin bfast?
  -- |]

  -- testLog [r|
  --   # 2025-02-05
  --   08:00 Breakfast
  --   09:00 Sanitizing moisture collector
  --   11:00 Exercising in high-grav gym
  -- |]

  -- testLog [r|
  --   -- wheee a comment

  --   # 2025-02-05
  --   08:00 Breakfast
  --   09:00 Sanitizing moisture collector -- should I try skippin bfast?
  --   11:00 Exercising in high-grav gym
  -- |]

  -- testLog example

  putStrLn "\n[Timespent per activity on day #1]"
  print $ timespent (result !! 0)

  putStrLn "\n[Timespent per activity on day #2]"
  print $ timespent (result !! 1)

  putStrLn "\n[Average timespent per activity per day]"
  print $ averagetimespent result

type Day   = Integer
type Month = Integer
type Year  = Integer

data Date = Date Year Month Day deriving (Show, Eq)

type Hour   = Integer
type Minute = Integer

data Time = Time Hour Minute deriving (Show, Eq)

newtype Total = Total { _total :: Time } deriving (Show, Eq)

instance Semigroup Total where
  (Total (Time h1 m1)) <> (Total (Time h2 m2)) =
    let t1 = h1 * 60 + m1
        t2 = h2 * 60 + m2
        (h', m') = (t1 + t2) `divMod` 60
    in  Total $ Time h' m'

instance Monoid Total where
  mempty = Total $ Time 0 0

data Task =
  Task { _date :: Date,
         _time :: Time,
         _name :: String  } deriving (Show, Eq)

comment :: Parser String
comment =
  string "--"
  >> oneOf " "
  >> many (noneOf "\n")
  <* whiteSpace

trailingComment :: Parser String
trailingComment = some (oneOf " ") >> comment

date :: Parser Date
date = do
  char '#'
  oneOf " "
  [yyyy, mm, dd] <- decimal `sepBy` char '-'
  return $ Date yyyy mm dd

time :: Parser Time
time = do
  [hh, mm] <- decimal `sepBy` colon
  return $ Time hh mm

task :: Date -> Parser Task
task d = do
  t <- time
  oneOf " "
  n <- manyTill (noneOf "\n")
                (try (void trailingComment <|> void newline))
  whiteSpace
  return $ Task d t n

daytasks :: Parser [Task]
daytasks = do
  d <- date <* skipOptional trailingComment
  whiteSpace
  option [] $ many (task d)

parseLog :: Parser [[Task]]
parseLog = do
  whiteSpace
  skipMany comment
  many daytasks

timespent :: [Task] -> [(String, Time)]
timespent ts = self ts
  where
    self (a:[])   =
      [(_name a, diff (_time a) (nextday . _time $ head ts))]
    self (a:b:xs) =
      (_name a, diff (_time a) (_time b)) : self (b:xs)
    nextday (Time h m) = Time (h + 24) m
    diff (Time h1 m1) (Time h2 m2) =
      let t1 = h1 * 60 + m1
          t2 = h2 * 60 + m2
          (h', m') = (t2 - t1) `divMod` 60
      in  Time h' m'

averagetimespent :: [[Task]] -> [(String, Time)]
averagetimespent =
  foldr average []                 .
  groupBy (\a b -> fst a == fst b) .
  sortOn fst                       .
  concatMap timespent
  where
    average :: [(String, Time)] -> [(String, Time)] -> [(String, Time)]
    average activities xs =
      let n = length activities
          (name, _)  = head activities
          (Time h m) = _total $ foldMap (Total . snd) activities
          (h', m')   = ((h * 60 + m) `div` toInteger n) `divMod` 60
      in  if n == 1
            then head activities : xs
            else (name, Time h' m') : xs
