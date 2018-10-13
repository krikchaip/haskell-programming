module MonadInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

testMonad :: IO ()
testMonad = do
  putStrLn "\n##### Nope #####"
  quickBatch $ functor (undefined :: Nope (Int, Int, Int))
  quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad (undefined :: Nope (Int, Int, Int))

  putStrLn "\n##### Either' #####"
  quickBatch $ functor (undefined :: Either' Int (Int, Int, Int))
  quickBatch $ applicative (undefined :: Either' Int (Int, Int, Int))
  quickBatch $ monad (undefined :: Either' Int (Int, Int, Int))

  putStrLn "\n##### Identity #####"
  quickBatch $ functor (undefined :: Identity (Int, Int, Int))
  quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
  quickBatch $ monad (undefined :: Identity (Int, Int, Int))

  putStrLn "\n##### List #####"
  quickBatch $ functor (undefined :: List (Int, Int, Int))
  quickBatch $ applicative (undefined :: List (Int, Int, Int))
  quickBatch $ monad (undefined :: List (Int, Int, Int))

-- ex01 -----------------------------------------
data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure  _   = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- ex02 -----------------------------------------
data Either' b a = Left' b | Right' a deriving (Show, Eq)

instance Functor (Either' b) where
  fmap _ (Left' b)  = Left' b
  fmap f (Right' a) = Right' $ f a

instance Applicative (Either' b) where
  pure = Right'
  (<*>) (Left' b) _           = Left' b
  (<*>) _         (Left' b)   = Left' b
  (<*>) (Right' f) (Right' a) = Right' $ f a

instance Monad (Either' b) where
  (Left' b)  >>= _ = Left' b
  (Right' a) >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (Either' b a) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Eq b, Eq a) => EqProp (Either' b a) where
  (=-=) = eq

-- ex03 -----------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- ex04 -----------------------------------------
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil         <*> _   = Nil
  _           <*> Nil = Nil
  (Cons f fs) <*> xs  = (f <$> xs) `append` (fs <*> xs)
    where
      append :: List a -> List a -> List a
      append Nil         ys  = ys
      append xs          Nil = xs
      append (Cons x xs) ys  = Cons x (xs `append` ys)

instance Monad List where
  Nil >>= _ = Nil
  xs  >>= f = join $ f <$> xs
    where
      join :: List (List a) -> List a
      join Nil = Nil
      join (Cons xs xss) = xs `append` (join xss)

      append :: List a -> List a -> List a
      append Nil         ys  = ys
      append xs          Nil = xs
      append (Cons x xs) ys  = Cons x (xs `append` ys)

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
