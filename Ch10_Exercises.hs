module Ch10_Exercises where

-- part01
stops  = "pbtdkg"
vowels = "aeiou"

stop_vowel_stop =
  [[x] ++ [y] ++ [z] | x <- stops, y <- vowels, z <- stops]

stop_vowel_stop' =
  [[x] ++ [y] ++ [z] | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["I", "You", "him", "her"]
verbs = ["love", "hate"]

noun_verb_noun =
  [ n ++ " " ++ v ++ " " ++ n'
    | n <- nouns,
      v <- verbs,
      n' <- nouns
  ]

-- part02
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFunc' :: Fractional a => String -> a
seekritFunc' x =
  let see = sum . map length . words $ x
      krit = length . words $ x
  in realToFrac see / realToFrac krit

-- part03
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\x' acc -> x == x' || acc) False

myElem' :: (Foldable t, Eq a) => a -> t a -> Bool
myElem' x = any (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy _ [x]    = x
myMaximumBy f (x:xs) = foldl localMax x xs
  where
    localMax cur acc = if f cur acc == GT then cur else acc

myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy f = myMaximumBy (flip f) . reverse
