module Ch10_DatabaseProcessing where

import Data.Time

data DatabaseItem =
  DbString String | DbNumber Integer | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
      (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
      (fromGregorian 1921 5 1)
      (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr toUTCTime [] where
  toUTCTime x acc = case x of
    DbDate time -> time : acc
    _           -> acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  let toDbNumber (DbNumber n) acc = n : acc
      toDbNumber  _           acc = acc
  in  foldr toDbNumber []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max mempty' . filterDbDate
  where mempty' = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = foldr ((\x acc -> x + acc / 2) . fromIntegral) 0 .
        filterDbNumber
