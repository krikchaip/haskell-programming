module EqInstances where

-- ex01
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

-- ex02
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  Two x y == Two x' y' = x == x' && y == y'

-- ex03
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt x)   (TisAnInt y)     = x == y
  (==) (TisAString s) (TisAString t)   = s == t
  (==) _              _                = False

-- ex04
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x' && y == y'

-- ex05
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' = x == x' && y == y'

-- ex06
data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  ThisOne x == ThisOne x' = x == x'
  ThatOne y == ThatOne y' = y == y'
  _         == _          = False

-- ex07
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello   x == Hello x'   = x == x'
  Goodbye y == Goodbye y' = y == y'
  _         == _          = False
