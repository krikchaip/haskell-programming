module Ch26_StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT g) =
    StateT $ \s -> fmap (\(a, _) -> (f a, s)) (g s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  -- Applicative variant (which typechecks, but wrong).
  -- (StateT f) <*> (StateT g) =
  --   StateT $ \s -> (\(h, _) (a, _) -> (h a, s)) <$> f s <*> g s
  --                                                   ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

  (StateT f) <*> (StateT g) =
    StateT $ \s -> do
      -- order-dependent computation
      (h, s') <- f s
      (a, s'') <- g s'
      return (h a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'
