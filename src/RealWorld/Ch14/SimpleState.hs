{-# LANGUAGE InstanceSigs #-}
module RealWorld.Ch14.SimpleState where

-- Abstract Data Type
data SimpleState s a = SimpleState (s -> (a, s))

-- Execution Function
runState :: SimpleState s a -> s -> (a, s)
runState (SimpleState f) s = f s

-- Helper Functions
get :: SimpleState s s
get = SimpleState (\s -> (s, s))

put :: s -> SimpleState s ()
put s = SimpleState (\_ -> ((), s))

-- Typeclass Instances
instance Functor (SimpleState s) where
  fmap :: (a -> b) -> SimpleState s a -> SimpleState s b
  fmap f statefulComp = SimpleState newFn
    where newFn s = let (a, s') = runState statefulComp s
                    in (f a, s')

instance Applicative (SimpleState s) where
  pure :: a -> SimpleState s a
  pure a = SimpleState (\s -> (a, s))

  (<*>) :: SimpleState s (a -> b) -> SimpleState s a -> SimpleState s b
  (<*>) statefulComp statefulComp' = SimpleState newFn
    where newFn s = let (f, s') = runState statefulComp s
                        (a, s'') = runState statefulComp' s'
                    in (f a, s'')

instance Monad (SimpleState s) where
  return :: a -> SimpleState s a
  return = pure

  (>>=) :: SimpleState s a -> (a -> SimpleState s b) -> SimpleState s b
  (>>=) statefulComp kont = SimpleState newFn
    where newFn s = let (a, s') = runState statefulComp s
                        statefulComp' = kont a
                    in runState statefulComp' s'
