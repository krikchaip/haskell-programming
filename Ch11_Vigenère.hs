module Ch11_Vigenère where

import Data.Char
import Data.List
import Control.Exception

-- uppercase both "keyword" and "target"
-- keyword must contain only alphabet letters (only a word)
-- don't encode spaces
vigenère :: String -> String -> String
vigenère keyword target =
  normalizedTarget `alphaShift` repeatedKeyword
  where normalizedTarget  = map toUpper target
        normalizedKeyword = map toUpper keyword
        repeatedKeyword   = repeatAsMessage normalizedKeyword normalizedTarget

alphaShift :: String -> String -> String
alphaShift = zipWith (\c alpha -> caesar (shift alpha) c)
  where shift a = case elemIndex a (map fst lowers) of
          Just i  -> i
          Nothing -> maybe 0 id $ elemIndex a (map fst uppers)

caesar :: Int -> Char -> Char
caesar n c
  | isAsciiLower c = chr . shiftInbound $ map snd lowers
  | isAsciiUpper c = chr . shiftInbound $ map snd uppers
  | otherwise      = c
  where
    shifted = ord c + n
    shiftInbound range
      | shifted > maximum range =
        range !! ((shifted - maximum range - 1) `mod` length range)
      | shifted < minimum range =
        range !! ((length range - minimum range + shifted) `mod` length range)
      | otherwise               = shifted

lowers :: [(Char, Int)]
lowers  = [(c, ord c) | c <- ['a'..'z']]

uppers :: [(Char, Int)]
uppers  = [(c, ord c) | c <- ['A'..'Z']]

repeatAsMessage :: String -> String -> String
repeatAsMessage pattern msg = unwords $ groupInto (words msg) stream
  where alphas = length . filter (not . isSpace) $ msg
        stream = take alphas . cycle $ pattern

groupInto :: [[a]] -> [b] -> [[b]]
groupInto [] _        = []
groupInto _  []       = []
groupInto (xs:xss) ys =
  let (y', y'') = splitAt (length xs) ys
  in  y' : groupInto xss y''

main :: IO ()
main = do
  -- let encoded   = vigenère "ALLY" "MEET AT DAWN"
  --     expected  = "MPPR AE OYWY"
  -- putStrLn $ assert (encoded == expected) "pass with \"ALLY\" keyword"
  putStr "Please enter a secret keyword: "
  keyword <- getLine
  putStr "Enter a message to be encode: "
  str <- getLine
  putStrLn $ "Encoded text: " ++ vigenère keyword str
