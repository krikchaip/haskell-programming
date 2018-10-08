module ApplicativeInstances where

-- Exercise: Identity Instance ------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

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
