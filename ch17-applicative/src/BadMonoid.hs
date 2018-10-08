module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty      = Fools

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = do
  -- We could also pass a bottom with a type assigned
  -- to let it know what to randomly generate for validating
  quickBatch $ monoid (undefined :: Bull)

  -- m :: type to test
  -- a, b, c :: are values to be using with some laws
  quickBatch $ applicative (undefined :: [] (String, String, Int))
