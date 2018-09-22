module Ch15_MonoidProperty where

import Data.Monoid
import Test.QuickCheck

type Associative a = a -> a -> a -> Bool
type Identity a = a -> Bool

-- Surprise! You can bind infix names for function arguments.
associativity :: Eq a => (a -> a -> a) -> Associative a
associativity (*) x y z =
  x * (y * z) == (x * y) * z

monoidAssociativity :: (Eq m, Monoid m) => Associative m
monoidAssociativity x y z =
  x `mappend` (y `mappend` z) == (x `mappend` y) `mappend` z

-- identity value on the left
monoidLeftIdentity :: (Eq m, Monoid m) => Identity m
monoidLeftIdentity a = (mempty `mappend` a) == a

-- identity value on the right
monoidRightIdentity :: (Eq m, Monoid m) => Identity m
monoidRightIdentity a = (a `mappend` mempty) == a

stringConcat :: String -> String -> String
stringConcat x y = x ++ y

check :: IO ()
check = do
  putStrLn "testing associativity for string concatenation."
  quickCheck $ associativity stringConcat

  putStrLn "testing monoid associativity for list of ints."
  quickCheck $ (monoidAssociativity :: Associative [Int])

  putStrLn "testing monoid left identity for Maybe String"
  quickCheck $ (monoidLeftIdentity :: Identity (Maybe String))

  putStrLn "testing monoid right identity for Maybe String"
  quickCheck $ (monoidRightIdentity :: Identity (Maybe String))
