module Morse
  ( Morse
  , charToMorse
  , morseToChar
  , stringToMorse
  , letterToMorse
  , morseToLetter
  ) where

import qualified Data.Map as M

type Morse = String

-- search letterToMorse with char
charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

-- search morseToLetter with morse
morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

-- not every Char that could potentially occur
-- in a String has a Morse representation
-- [Maybe Morse] -> Maybe [Morse]
stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList
  [ ('a', ".-"),    ('b', "-..."),  ('c', "-.-."),  ('d', "-.."),   ('e', ".")
  , ('f', "..-."),  ('g', "--."),   ('h', "...."),  ('i', ".."),    ('j', ".---")
  , ('k', "-.-"),   ('l', ".-.."),  ('m', "--"),    ('n', "-."),    ('o', "---")
  , ('p', ".--."),  ('q', "--.-"),  ('r', ".-."),   ('s', "..."),   ('t', "-")
  , ('u', "..-"),   ('v', "...-"),  ('w', ".--"),   ('x', "-..-"),  ('y', "-.--")
  , ('z', "--.."),  ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-")
  , ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----.")
  , ('0', "-----")
  ]

-- fold with \key value acc -> acc
-- [(".-", 'a'), ("-...", 'b'), ...]
morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse
