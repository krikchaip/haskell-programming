{-# LANGUAGE InstanceSigs #-}

module Ch25_Twinplicative where

identity :: a -> a
identity a = a

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g a = f (g a)

-- Identity :: * -> *
newtype Identity a =
  Identity { getIdentity :: a }
  deriving (Show, Eq)

-- Compose :: (* -> *) -> (* -> *) -> * -> *
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Show, Eq)

instance (Functor f, Functor g)
      => Functor (Compose f g) where

  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
      => Applicative (Compose f g) where

  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose fgab) <*> (Compose fga) =
    Compose $ pure (<*>) <*> fgab <*> fga
