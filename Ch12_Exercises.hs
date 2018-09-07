module Ch12_Exercises where

notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe ""       = ""
replaceThe s@(c:cs)
  | c == ' '  = c : replaceThe cs
  | otherwise = case notThe word of
    Just _  -> word ++ replaceThe s'
    Nothing -> "a"  ++ replaceThe s'
  where (word, s') = span (/=' ') s

-- I could turn "headOrNil" or "tailOrNil" into "Maybe"-
-- returned function ... but why i have to? :p
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel ""       = 0
countTheBeforeVowel s@(c:cs)
  | c == ' '  = countTheBeforeVowel cs
  | otherwise = case notThe word of
    Just _  -> countTheBeforeVowel s'
    Nothing -> if headOrNil next `elem` "aeiou"
      then 1 + countTheBeforeVowel s''
      else countTheBeforeVowel s''
  where (word, s')  = span (/=' ') s
        (next, s'') = span (/=' ') $ tailOrNil s'

headOrNil :: String -> Char
headOrNil ""    = '\0'
headOrNil (x:_) = x

tailOrNil :: String -> String
tailOrNil ""     = ""
tailOrNil (_:[]) = ""
tailOrNil (_:xs) = xs

-- doesn't explicitly tell me to write recursive function,
-- so i use foldr
countVowels :: String -> Integer
countVowels = (flip foldr) 0
  (\c acc -> if c `elem` "aeiou" then 1 + acc else acc)

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s =
  if nvowels > nconsonants
    then Nothing
    else Just (Word' s)
  where nvowels     = length [a | a <- s, a `elem` vowels]
        nconsonants = length s - nvowels

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0 = Just Zero
  | n <  0 = Nothing
  | n >  0 = Just $ go n
  where go 0 = Zero
        go n = Succ $ go (n - 1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee y _ Nothing  = y
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x' Nothing   = x'
fromMaybe x' (Just x)  = x

fromMaybe' :: b -> Maybe b -> b
fromMaybe' x' = mayybee x' id

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes []           = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs)  = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []           = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe (Just x:xs)  = Just x `cons` flipMaybe xs
  where cons Nothing  _         = Nothing
        cons _        Nothing   = Nothing
        cons (Just x) (Just xs) = Just (x:xs)

lefts' :: [Either a b] -> [a]
lefts' = foldr
  (\mx acc -> case mx of
    Left a  -> a : acc
    Right b -> acc)
  []

rights' :: [Either a b] -> [b]
rights' = foldr
  (\mx acc -> case mx of
    Left a  -> acc
    Right b -> b : acc)
  []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case result of
  Nothing     -> []
  Just (a, b) -> a : myUnfoldr f b
  where result = f x

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- tree generator with unfold
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case result of
  Nothing          -> Leaf
  Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)
  where result = f x

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold
  (\x -> if x >= n
    then Nothing else Just (x+1, x, x+1))
  0
