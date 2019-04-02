module Ch26_EitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (EitherT f) <*> (EitherT x) = EitherT $ (<*>) <$> f <*> x

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left  e -> return (Left e)
      Right a -> runEitherT . f $ a

-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swap <$> ema
  where swap :: Either a b -> Either b a
        swap (Left a)  = Right a
        swap (Right b) = Left b

-- the transformer variant of the either catamorphism.
-- note: the only reason why we can't simply return 'c'
--       because we don't have any information about 'm'
eitherT :: Monad m
        => (a -> m c) -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT m) = do
  ab <- m
  case ab of
    Left  a -> f a
    Right b -> g b
