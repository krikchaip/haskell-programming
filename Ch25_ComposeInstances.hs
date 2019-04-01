{-# LANGUAGE InstanceSigs #-}

module Ch25_ComposeInstances where

import Ch25_Twinplicative (Compose(..))

instance (Foldable f, Foldable g)
      => Foldable (Compose f g) where

  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap am (Compose fga) = (foldMap . foldMap) am fga

instance (Traversable f, Traversable g)
      => Traversable (Compose f g) where

  traverse :: Applicative h
    => (a -> h b)
    -> Compose f g a
    -> h (Compose f g b)
  traverse f (Compose fga) =
    Compose <$> (traverse . traverse) f fga
