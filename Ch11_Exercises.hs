module Ch11_Exercises where

import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf xs' xs = xs' ==
  foldr (\x acc -> if x `elem` xs' then x : acc else acc) [] xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = [(w, capitalizeWord w) | w <- words s]

capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (c:cs)
  | isAlpha c = toUpper c : cs
  | otherwise = c : capitalizeWord cs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph s  =
  capitalizeWord s' ++ [dot] ++ capitalizeParagraph s''
  where (s', dot:s'') = break (=='.') s
