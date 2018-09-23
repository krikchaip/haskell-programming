module Ch15_Exercises where

import Test.QuickCheck hiding (Success, Failure)

import Data.Monoid
import Control.Applicative (liftA2, liftA3)

type Associative a = a -> a -> a -> Bool
type MIdentity a = a -> Bool

check :: IO ()
check = do
  -- ex01 - Semigroup, Monoid
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: Associative Trivial)
  quickCheck (monoidLeftIdentity :: MIdentity Trivial)
  quickCheck (monoidRightIdentity :: MIdentity Trivial)

  -- ex02 - Semigroup, Monoid
  putStrLn "Identity"
  quickCheck (semigroupAssoc :: Associative (Identity String))
  quickCheck (monoidLeftIdentity :: MIdentity (Identity String))
  quickCheck (monoidRightIdentity :: MIdentity (Identity String))

  -- ex03 - Semigroup, Monoid
  putStrLn "Two"
  quickCheck (semigroupAssoc :: Associative (Two All (Sum Int)))
  quickCheck (monoidLeftIdentity :: MIdentity (Two All (Sum Int)))
  quickCheck (monoidRightIdentity :: MIdentity (Two All (Sum Int)))

  putStrLn "Three"
  quickCheck (semigroupAssoc :: Associative (Three All (Sum Int) (Product Int))) -- ex04

  putStrLn "Four"
  quickCheck (semigroupAssoc :: Associative (Four All Any (Sum Int) (Product Int))) -- ex05

  -- ex06 - Semigroup, Monoid
  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: Associative BoolConj)
  quickCheck (monoidLeftIdentity :: MIdentity BoolConj)
  quickCheck (monoidRightIdentity :: MIdentity BoolConj)

  -- ex07 - Semigroup, Monoid
  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: Associative BoolDisj)
  quickCheck (monoidLeftIdentity :: MIdentity BoolDisj)
  quickCheck (monoidRightIdentity :: MIdentity BoolDisj)

  putStrLn "Or"
  quickCheck (semigroupAssoc :: Associative (Or Char Int)) -- ex08

  -- ex09 - Semigroup, Monoid
  putStrLn "Combine"
  let semigroupAssocCombine x a b c =
        (==) (unCombine (a <> (b <> c)) $ x)
             (unCombine ((a <> b) <> c) $ x)
      monoidLeftIdentityCombine x a =
        (==) (unCombine (mempty `mappend` a) $ x)
             (unCombine a $ x)
      monoidRightIdentityCombine x a =
        (==) (unCombine (a `mappend` mempty) $ x)
             (unCombine a $ x)
  quickCheck (semigroupAssocCombine :: Bool -> Associative (Combine Bool (Product Int)))
  quickCheck (monoidLeftIdentityCombine :: Bool -> MIdentity (Combine Bool (Product Int)))
  quickCheck (monoidRightIdentityCombine :: Bool -> MIdentity (Combine Bool (Product Int)))

  -- ex10 - Semigroup, Monoid
  putStrLn "Comp"
  let semigroupAssocComp x a b c =
        (==) (unComp (a <> (b <> c)) $ x)
             (unComp ((a <> b) <> c) $ x)
      monoidLeftIdentityComp x a =
        (==) (unComp (mempty `mappend` a) $ x)
             (unComp a $ x)
      monoidRightIdentityComp x a =
        (==) (unComp (a `mappend` mempty) $ x)
             (unComp a $ x)
  quickCheck (semigroupAssocComp :: Int -> Associative (Comp Int))
  quickCheck (monoidLeftIdentityComp :: Int -> MIdentity (Comp Int))
  quickCheck (monoidRightIdentityComp :: Int -> MIdentity (Comp Int))

  putStrLn "Validation"
  quickCheck (semigroupAssoc :: Associative (Validation (Product Int) Char)) -- ex11

  -- ex12 - Semigroup, Monoid
  putStrLn "Mem"
  let f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

semigroupAssoc :: (Eq m, Semigroup m) => Associative m
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => MIdentity m
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => MIdentity m
monoidRightIdentity a = (a `mappend` mempty) == a

-- ex01 - Semigroup, Monoid
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- ex02 - Semigroup, Monoid
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- ex03 - Semigroup, Monoid
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two (mempty) (mempty)

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

-- ex06 - Semigroup, Monoid
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _               <> _               = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- ex07 - Semigroup, Monoid
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _                <> _                = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- ex08 - Semigroup
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst _) <> o' = o'
  o       <> _  = o

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

-- ex09 - Semigroup, Monoid
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine function here"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- ex10 - Semigroup, Monoid
newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp function here"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (g . f)

instance Monoid (Comp a) where
  mempty = Comp id

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

-- ex12 - Semigroup, Monoid
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ \x ->
    let fa = fst . f
        ga = fst . g
        fs = snd . f
        gs = snd . g
    in  (fa x <> ga x, gs . fs $ x)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)
