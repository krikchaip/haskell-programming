module IPv4 where

import Text.Trifecta

import Data.Word
import Data.Bits

data IPv4 =
  IPv4 Word32 Word32 Word32 Word32
  deriving (Eq, Ord, Show)

toDecimal :: IPv4 -> Word32
toDecimal (IPv4 aaa bbb ccc ddd)
  = sum [ aaa `shiftL` 24,
          bbb `shiftL` 16,
          ccc `shiftL` 8,
          ddd ]

ipv4 :: Parser IPv4
ipv4 = do
  aaa <- octet <* char '.'
  bbb <- octet <* char '.'
  ccc <- octet <* char '.'
  ddd <- octet
  return $ IPv4 aaa bbb ccc ddd
  where
    octet :: Parser Word32
    octet = do
      n <- decimal
      if 0 <= n && n <= 255
        then return (fromInteger n)
        else unexpected "IPv4 must be in [0,255]"

example1 :: String
example1 = "172.16.254.1"

example2 :: String
example2 = "204.120.0.15"
