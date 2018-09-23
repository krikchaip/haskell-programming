module Ch15_Exercises where

import Test.QuickCheck hiding (Success, Failure)

import Data.Monoid
import Control.Applicative (liftA2, liftA3)

type Associative a = a -> a -> a -> Bool

semigroups :: IO ()
semigroups = do
  quickCheck (semigroupAssoc :: Associative Trivial) -- ex01
  quickCheck (semigroupAssoc :: Associative (Identity String)) -- ex02
  quickCheck (semigroupAssoc :: Associative (Two All (Sum Int))) -- ex03
  quickCheck (semigroupAssoc :: Associative (Three All (Sum Int) (Product Int))) -- ex04
  quickCheck (semigroupAssoc :: Associative (Four All Any (Sum Int) (Product Int))) -- ex05
  quickCheck (semigroupAssoc :: Associative BoolConj) -- ex06
  quickCheck (semigroupAssoc :: Associative BoolDisj) -- ex07
  quickCheck (semigroupAssoc :: Associative (Or Char Int)) -- ex08

  -- ex09
  let semigroupAssocCombine :: (Eq b, Semigroup b) => a -> Associative (Combine a b)
      semigroupAssocCombine x a b c =
        (==) (unCombine (a <> (b <> c)) $ x)
             (unCombine ((a <> b) <> c) $ x)
  quickCheck (semigroupAssocCombine :: Bool -> Associative (Combine Bool (Product Int)))

  -- ex10
  let semigroupAssocComp :: Eq a => a -> Associative (Comp a)
      semigroupAssocComp x a b c =
        (==) (unComp (a <> (b <> c)) $ x)
             (unComp ((a <> b) <> c) $ x)
  quickCheck (semigroupAssocComp :: Int -> Associative (Comp Int))

  quickCheck (semigroupAssoc :: Associative (Validation (Product Int) Char)) -- ex11

semigroupAssoc :: (Eq m, Semigroup m) => Associative m
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- ex01 - Semigroup
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- ex02 - Semigroup
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- ex03 - Semigroup
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

-- ex04 - Semigroup
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
          Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
          Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

-- ex05 - Semigroup
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
        Semigroup (Four a b c d) where
  (Four x y z aa) <> (Four x' y' z' aa') =
    Four (x <> x') (y <> y') (z <> z') (aa <> aa')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
          Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- ex06 - Semigroup
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _               <> _               = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- ex07 - Semigroup
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _                <> _                = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- ex08 - Semigroup
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst _) <> o' = o'
  o       <> _  = o

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

-- ex09 - Semigroup
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine function here"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- ex10 - Semigroup
newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp function here"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (g . f)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

-- ex11 - Semigroup
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  s@(Success _) <> (Failure _)   = s
  (Failure a)   <> (Failure b)   = Failure $ a <> b
  s@(Success _) <> (Success _)   = s
  (Failure _)   <> s@(Success _) = s

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]
