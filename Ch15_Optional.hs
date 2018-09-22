module Ch15_Optional where

import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

instance Semigroup (First' a) where
  (First' (Only x)) <> (First' (Only y)) = First' (Only x)
  First' Nada       <> foy               = foy
  fox               <> First' Nada       = fox

instance Monoid (First' a) where
  mempty = First' Nada

instance Monoid a => Semigroup (Optional a) where
  (Only x) <> (Only y) = Only $ x `mappend` y
  Nada     <> oy       = oy
  ox       <> Nada     = ox

-- only one Monoid definition for Optional a
instance Monoid a => Monoid (Optional a) where
  mempty = Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

monoidAssoc x y z =
  x `mappend` (y `mappend` z) == (x `mappend` y) `mappend` z

monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity a = (a `mappend` mempty) == a

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, return $ First' Nada)
                        ,(1, First' . Only <$> arbitrary)]

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
