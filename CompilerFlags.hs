-- If you had not included the NoMonomorphismRestriction extension,
-- x would have had the type Integer instead of Num a => a.
{-# LANGUAGE NoMonomorphismRestriction #-}

-- allow newtype to derive on a typeclass
-- for which the type it contains
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- allow writing an instance of typeclass
-- for a type like (Int, String), [Bool], etc.
-- without creating newtypes
{-# LANGUAGE FlexibleInstances #-}

module CompilerFlags where

-- @ LANGUAGE NoMonomorphismRestriction
-- try toggle comment on line 3 then hover on x to see effects
x = 1

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int
  where tooMany n = n > 42

-- @ LANGUAGE GeneralizedNewtypeDeriving
-- instead of:
-- newtype Goats = Goats Int deriving (Eq, Show)
-- instance TooMany Goats where
--   tooMany (Goats n) = tooMany n
newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

-- @ LANGUAGE FlexibleInstances
-- should allow this instead of writing:
-- newtype Foo = Foo (Int, String)
-- instance TooMany Foo where
--   tooMany (Foo ...) = ...
instance TooMany (Int, String) where
  tooMany (n, _) = n > 20
