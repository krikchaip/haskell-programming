module Ch09_EFT where

eftBool :: Bool -> Bool -> [Bool]
eftBool from to
  | from > to  = []
  | from == to = [to]
  | otherwise  = from : eftBool (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to
  | from > to  = []
  | from == to = [to]
  | otherwise  = from : eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from > to  = []
  | from == to = [to]
  | otherwise  = from : eftInt (succ from) to

eftChar :: Char -> Char -> [Char]
eftChar from to
  | from > to  = []
  | from == to = [to]
  | otherwise  = from : eftChar (succ from) to

eft :: (Ord t, Enum t) => t -> t -> [t]
eft from to
  | from > to  = []
  | from == to = [to]
  | otherwise  = from : eft (succ from) to
