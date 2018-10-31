-- {-# LANGUAGE FlexibleContexts #-}

module TraversableInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "\n##### Identity #####"
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))

  putStrLn "\n##### Constant #####"
  quickBatch $ traversable (undefined :: Constant Int (Int, Int, [Int]))

  putStrLn "\n##### Optional #####"
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))

  putStrLn "\n##### List #####"
  quickBatch $ traversable (undefined :: List (Int, Int, [Int]))

  putStrLn "\n##### Three #####"
  quickBatch $ traversable (undefined :: Three Int Int (Int, Int, [Int]))

  putStrLn "\n##### Pair #####"
  quickBatch $ traversable (undefined :: Pair Int (Int, Int, [Int]))

  putStrLn "\n##### Big #####"
  quickBatch $ traversable (undefined :: Big Int (Int, Int, [Int]))

  putStrLn "\n##### Tree #####"
  quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))

-- Identity ---------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

-- Constant ---------------------------------------------------------
newtype Constant a b = Constant { getConstant :: a } deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

-- Optional ---------------------------------------------------------
data Optional a = Nada | Yep a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance Functor Optional where
  fmap _ Nada     = Nada
  fmap f (Yep a)  = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

-- List -------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromList <$> arbitrary where
    fromList :: [a] -> List a
    fromList = foldr Cons Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldr _ m Nil         = m
  foldr f m (Cons a as) = a `f` (foldr f m as)

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> (traverse f as)

-- Three ------------------------------------------------------------
data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- Pair -------------------------------------------------------------
data Pair a b = Pair a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

-- Big --------------------------------------------------------------
data Big a b = Big a b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary
                  <*> arbitrary
                  <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

-- Tree -------------------------------------------------------------
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [ return Empty
                    , Leaf <$> arbitrary
                    , Node <$> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance (Eq a) => EqProp (Tree a) where (=-=) = eq

instance Functor Tree where
  fmap _ Empty           = Empty
  fmap f (Leaf a)        = Leaf (f a)
  fmap f (Node ta a ta') = Node (f <$> ta) (f a) (f <$> ta')

instance Foldable Tree where
  foldMap _ Empty           = mempty
  foldMap f (Leaf a)        = f a
  foldMap f (Node ta a ta') = (foldMap f ta) <> f a <> (foldMap f ta')

instance Traversable Tree where
  traverse _ Empty           = pure Empty
  traverse f (Leaf a)        = Leaf <$> f a
  traverse f (Node ta a ta') = Node <$> (traverse f ta) <*> (f a) <*> (traverse f ta')
