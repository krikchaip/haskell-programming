{-# LANGUAGE KindSignatures, InstanceSigs #-}

module Ch25_Bifunctor where

class Bifunctor (p :: * -> * -> *) where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
data Const a b = Const a
data Drei a b c = Drei a b c
data SuperDrei a b c = SuperDrei a b
data SemiDrei a b c = SemiDrei a
data Quadriceps a b c d = Quadzzz a b c d
data Either a b = Left a | Right b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Ch25_Bifunctor.Either where
  bimap f _ (Ch25_Bifunctor.Left a) = Ch25_Bifunctor.Left (f a)
  bimap _ g (Ch25_Bifunctor.Right b) = Ch25_Bifunctor.Right (g b)
