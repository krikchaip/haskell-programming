module IPv4 where

import Text.Trifecta

import IPv6 (IPAddress6(..), toDecimal, data1)

import Data.Word
import Data.Bits
import Data.List

data IPv4 =
  IPv4 Word8 Word8 Word8 Word8
  deriving (Eq, Ord, Show)

newtype IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress n) =
    let shifts = [24, 16, 8, 0]
    in  intercalate "." $ show . (.&. 255) . (n `shiftR`) <$> shifts

toDecimal :: IPv4 -> IPAddress
toDecimal (IPv4 aaa bbb ccc ddd)
  = let shifts = [24, 16, 8, 0]
        xs = fromIntegral <$> [aaa, bbb, ccc, ddd]
    in  IPAddress $ sum (zipWith shiftL xs shifts)

toV6 :: IPAddress -> IPAddress6
toV6 (IPAddress n) =
  IPAddress6 0 (fromIntegral n)

fromV6 :: IPAddress6 -> IPAddress
fromV6 (IPAddress6 _ b) =
  IPAddress . (.&. (2 ^ 32 - 1)) . fromIntegral $ b

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

example3 :: IPv4
example3 = IPv4 172 16 254 1
