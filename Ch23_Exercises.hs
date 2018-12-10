module Ch23_Exercises where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  f `fmap` (State g) = State $ \s ->
    let (a, ns) = g s
    in  (f a, ns)

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (State sab) <*> (State sa) = State $ \s ->
    let (f, _) = sab s
        (x, _) = sa s
    in  (f x, s)

instance Monad (State s) where
  (State sa) >>= f = State $ \s ->
    let (x, s') = sa s
    in  runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec sa = snd . runState sa

eval :: State s a -> s -> a
eval sa = fst . runState sa

modify :: (s -> s) -> State s ()
modify f = get >>= (put . f)
