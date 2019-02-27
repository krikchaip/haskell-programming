{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ch24_Marshalling2 where

-- import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
-- import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

data NumberOrString =
    Numba Integer
  | Stringy Text
  deriving (Eq, Show)

-- aeson choosed Scientific as JS's Number representation
-- so we must somehow convert to Integer first.
instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) ->
        fail "Must be integral number"
      (Right integer) ->
        return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ =
    fail "NumberOrString must\
         \ be number or string"

-- so it knows what we want to parse
dec :: ByteString
    -> Maybe NumberOrString
dec = decode

-- using `eitherDec` slightly get a more helpful type error
eitherDec :: ByteString
          -> Either String NumberOrString
eitherDec = eitherDecode

main :: IO ()
main = do
  print $ dec "blah"
  print $ eitherDec "blah" -- eitherDec come to rescue!
  print $ dec "123"
  print $ dec "\"blah\"" -- the right one
  print $ dec [r|"blah"|] -- alternative
