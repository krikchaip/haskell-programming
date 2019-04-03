module Ch26_LiftMore where

import Control.Monad (liftM)
import Control.Monad.Trans.Class

import Ch26_EitherT (EitherT(..))
import Ch26_StateT (StateT(..))

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM return

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
