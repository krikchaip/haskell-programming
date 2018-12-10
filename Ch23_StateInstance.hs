module Ch23_StateInstance where

newtype ST s a = ST { runST :: s -> (a, s) }

instance Functor (ST s) where
  f `fmap` (ST g) = ST $ \s ->
    let (a, ns) = g s
    in  (f a, ns)

-- runST ((+1) <$> (ST $ \s -> (0, s))) 0

instance Applicative (ST s) where
  pure x = ST $ \s -> (x, s)
  (ST sab) <*> (ST sa) = ST $ \s ->
    let (f, _) = sab s
        (x, _) = sa s
    in  (f x, s)

instance Monad (ST s) where
  (ST sa) >>= f = ST $ \s ->
    let (x, s') = sa s
    in  runST (f x) s'
