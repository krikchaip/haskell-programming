module Ch11_LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Foo = Foo (Int, String)
instance TooMany Foo where
  tooMany (Foo (n, _)) = n > 42

newtype Bar = Bar (Int, Int)
instance TooMany Bar where
  tooMany (Bar (x, y)) = x + y > 42
