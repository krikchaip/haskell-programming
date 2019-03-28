module IPv4 where

import Text.Trifecta

import Data.Word
import Data.Bits
import Data.List

data IPv4 =
  IPv4 Word8 Word8 Word8 Word8
  deriving (Eq, Ord, Show)

newtype IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

toDecimal :: IPv4 -> IPAddress
toDecimal (IPv4 aaa bbb ccc ddd)
  = let shifts = [24, 16, 8, 0]
        xs = fromIntegral <$> [aaa, bbb, ccc, ddd]
    in  IPAddress $ head (zipWith shiftL xs shifts)

ipv4 :: Parser IPv4
ipv4 =
  IPv4 <$> octet <* char '.'
       <*> octet <* char '.'
       <*> octet <* char '.'
       <*> octet
  where
    octet :: Parser Word8
    octet = do
      n <- decimal
      if 0 <= n && n <= 255
        then return (fromInteger n)
        else unexpected "IPv4 must be in [0,255]"

example1 :: String
example1 = "172.16.254.1"

example2 :: String
example2 = "204.120.0.15"
