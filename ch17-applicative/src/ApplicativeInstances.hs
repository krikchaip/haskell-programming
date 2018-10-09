module ApplicativeInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main = do
  putStrLn "\n### Identity ###"
  quickBatch $ applicative (undefined :: Identity (Int, Int, Int))

  putStrLn "\n### Constant ###"
  quickBatch $ applicative (undefined :: Constant String (Int, Int, Int))

  putStrLn "\n### List ###"
  quickBatch $ applicative (undefined :: List (Int, Int, Int))

  putStrLn "\n### Validation ###"
  quickBatch $ applicative (undefined :: Validation [String] (Int, Int, Int))

-- Exercise: Identity Instance ------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Exercise: Constant Instance ------------------------------------------------
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  -- you can't write `fmap _ ca = ca`
  -- otherwise Haskell couldn't assume the type
  -- fmap :: (a -> b) -> Constant e a -> Constant e b
  fmap _ ca = Constant $ getConstant ca

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty

  -- The function doesn’t exist, and the 𝑏 is a ghost.
  (Constant x) <*> (Constant y) = Constant (x `mappend` y)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-- Exercise: List Instance ----------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil -- preserve structure
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil         <*> _   = Nil
  _           <*> Nil = Nil
  (Cons f af) <*> ax  = (f <$> ax) `append` (af <*> ax)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ return Nil, Cons <$> arbitrary <*> arbitrary ]

instance Eq a => EqProp (List a) where
  xs =-= ys =
    let take' n xs = case (n, xs) of
          (0, _)          -> Nil
          (_, Nil)        -> Nil
          (_, Cons x xs') -> Cons x (take' (n - 1) xs')
        takeAll = take' 100
    in  takeAll xs `eq` takeAll ys

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

-- Exercise: Validation Instance ----------------------------------------------
data Validation e a = Fail e | Succ a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  _ `fmap` (Fail e) = Fail e
  f `fmap` (Succ a) = Succ $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Succ
  (Fail e) <*> (Fail e') = Fail $ e <> e'
  (Fail e) <*> (Succ _)  = Fail e
  (Succ _) <*> (Fail e)  = Fail e
  (Succ f) <*> (Succ x)  = Succ $ f x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [ Fail <$> arbitrary, Succ <$> arbitrary ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq
