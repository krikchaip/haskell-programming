{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Ch24_Marshalling where

import Control.Applicative ((<|>))
import Data.Aeson
import Text.RawString.QQ
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

sectionJson :: BS.ByteString
sectionJson = [r|
  {
    "section": {
      "host": "wikipedia.org"
    },
    "whatisit": {
      "red": "intoothandclaw"
    }
  }
|]

sectionJson' :: LBS.ByteString
sectionJson' = [r|
  {
    "section": {
      "host": "wikipedia.org"
    },
    "whatisit": {
      "red": "intoothandclaw"
    }
  }
|]

type Annotation = String

newtype Host = Host String deriving (Eq, Show)

data Color =
    Red Annotation
  | Blue Annotation
  | Yellow Annotation deriving (Eq, Show)

data TestData =
  TestData {
    section :: Host
  , what    :: Color
  } deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ =
    fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "host"
  parseJSON _ =
    fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
        (Red    <$> v .: "red")
    <|> (Blue   <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ =
    fail "Expected an object for Color"

main :: IO ()
main = do
  let blah :: Maybe Value
      blah = decodeStrict sectionJson

      -- ต้องเขียน instance FromJSON เพื่อเปลี่ยน Value -> TestData
      d :: Maybe TestData
      d = decode sectionJson'
  print blah
  print d
