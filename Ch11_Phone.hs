module Ch11_Phone where

import Data.Char
import Data.Maybe
import Data.List

newtype DaPhone = DaPhone [Button]

data Button = Button Digit Variant

type Digit   = Char   -- valid buttons = "1234567890*#"
type Variant = String -- possible characters
type Presses = Int    -- valid presses: 1 and up

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn" ]

iPhone :: DaPhone
iPhone = DaPhone
  [ Button '0' "+ 0"
  , Button '1' "1"
  , Button '2' "abc2"
  , Button '3' "def3"
  , Button '4' "ghi4"
  , Button '5' "jkl5"
  , Button '6' "mno6"
  , Button '7' "pqrs7"
  , Button '8' "tuv8"
  , Button '9' "wxyz9"
  , Button '*' "^*"
  , Button '#' ".,#"
  ]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone buttons) c
  | isAsciiUpper c = reverseTaps phone '^' ++ reverseTaps phone (toLower c)
  | otherwise      = [(d, press c p) | Button d p <- buttons, c `elem` p]
  where press c = (+1) . fromMaybe 0 . elemIndex c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\x acc -> snd x + acc) 0

mostPopularLetter :: String -> Char
mostPopularLetter = fst . maximumBy counts . countLetter . filter isLetter
  where counts a b  = snd a `compare` snd b
        countLetter = foldr mergeCount [] . sortOn fst . map (\x -> (x, 1))

mostPopularWord :: [String] -> String
mostPopularWord = fst . maximumBy counts . countWord . map normalize
  where counts a b  = snd a `compare` snd b
        normalize = map toLower
        countWord = foldr mergeCount [] . sortOn fst . map (\x -> (x, 1))

mergeCount :: (Eq a, Num b) => (a, b) -> [(a, b)] -> [(a, b)]
mergeCount cn        []                = [cn]
mergeCount cn@(c, n) acc@((c', n'):xs) =
  if c == c' then (c', n + n') : xs else cn : acc

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopularWord . allWords
  where allWords = concatMap words
