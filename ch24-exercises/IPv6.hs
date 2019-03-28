module IPv6 where

import Text.Trifecta

import Data.Word
import Data.List
import Data.Bits
import Data.Function ((&))
import Numeric

data IPv6 =
  IPv6 Word16 Word16 Word16 Word16
       Word16 Word16 Word16 Word16
  deriving (Eq, Ord, Show)

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

toDecimal :: IPv6 -> IPAddress6
toDecimal (IPv6 aaaa bbbb cccc dddd
                eeee ffff gggg hhhh) =
  let (first, second) =
        splitAt 4 $ fromIntegral <$>
          [ aaaa, bbbb, cccc, dddd
          , eeee, ffff, gggg, hhhh ]
      shiftLZip = zipWith (flip shiftL) [48, 32, 16, 0]
  in  IPAddress6 (sum (shiftLZip first))
                 (sum (shiftLZip second))

ipv6 :: Parser IPv6
ipv6 = do
  let isNil a b = null a && null b
  xs <- nubBy isNil <$> many hexDigit `sepBy1` char ':'
  let padded =
        if and [length xs == 8, "" `notElem` xs]
        then xs
        else fill0s xs
      fill0s xs = concatMap
        (\hex ->
          if null hex
          then replicate (8 - length xs + 1) "0"
          else [hex]) xs
      [ aaaa, bbbb, cccc, dddd,
        eeee, ffff, gggg, hhhh ]
        = padded & concatMap readHex & fmap fst
  return $ IPv6 aaaa bbbb cccc dddd
                eeee ffff gggg hhhh

-- 0:0:0:0:0:ffff:ac10:fe01 -> 281473568538113
data1 :: IPv6
data1 =
  IPv6 0     0     0     0
       0 65535 44048 65025

-- 0:0:0:0:0:ffff:cc78:f -> 281474112159759
data2 :: IPv6
data2 =
  IPv6 0     0     0  0
       0 65535 52344 15

data3 :: String
data3 = "0:0:0:0:0:ffff:ac10:fe01"

data4 :: String
data4 = "0:0:0:0:0:ffff:cc78:f"

data5 :: String
data5 = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"

data6 :: String
data6 = "2001:DB8::8:800:200C:417A"

data7 :: String
data7 = "FE80::0202:B3FF:FE1E:8329"

data8 :: String
data8 = "2001:DB8::8:800:200C:417A"
