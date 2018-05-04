module Lib where


-- +-------+
-- + State +
-- +-------+

newtype State s a = State { runState :: s -> (a, s) }

instance Show (State s a) where
  show _ = "State s a"


-- | Apply the given function f to the first item of a pair.
_ap f (a, b) = (f a, b)


instance Functor (State s) where
  f `fmap` st = state $ _ap f . runState st


instance Applicative (State s) where
  pure = State . (,)
  a <*> b = state $ \s -> let (f, s') = runState a s in _ap f (runState b s')


instance Monad (State s) where
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: StateT s a -> (a -> StateT s b) -> StateT s b
  -- (>>=) :: (s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s))
  m >>= k = state $ \s -> let (a, s') = runState m s in runState (k a) s'


-- +-----------+
-- + State fns +
-- +-----------+

state :: (s -> (a, s)) -> State s a
state = State


push :: a -> State [a] ()
push x = state $ \xs -> ((), x : xs)


pop :: State [a] a
pop = state $ \(x : xs) -> (x, xs)


put :: s -> State s ()
put s = state $ const ((), s)


get :: State s s
get = state $ \s -> (s, s)


gets :: (s -> a) -> State s a
gets f = state $ \s -> (f s, s)


modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)


evalState :: State s a -> s -> a
evalState w s = fst $ runState w s


execState :: State s a -> s -> s
execState w s = snd $ runState w s


-- +--------+
-- + StateT +
-- +--------+

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  -- fmap f t = StateT $ \s -> g <$> (runStateT t s)
  fmap f t = StateT $ fmap g . runStateT t
    where g (a, s) = (f a, s)


instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> return (x, s)
  StateT mf <*> StateT ma = StateT $ \s -> do
    (g, s')  <- mf s
    (x, s'') <- ma s'
    return (g x, s'')


instance (Monad m) => Monad (StateT s m) where
  -- StateT ma >>= k = StateT $ \s -> ma s >>= \(a, s') -> runStateT (k a) s'
  StateT ma >>= k = StateT $ \s -> do
    (a, s') <- ma s
    runStateT (k a) s'

