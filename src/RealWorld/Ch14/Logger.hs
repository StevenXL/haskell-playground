{-# LANGUAGE InstanceSigs #-}

module RealWorld.Ch14.Logger where

type Log = [String]

data Logger a = Logger { execLogger :: (a, Log) }

instance Functor Logger where
  fmap :: (a -> b) -> Logger a -> Logger b
  fmap f (Logger (a, l)) = Logger (f a, l)

instance Applicative Logger where
  pure :: a -> Logger a
  pure a = Logger (a, [])

  (<*>) :: Logger (a -> b) -> Logger a -> Logger b
  (<*>) (Logger (f, l)) (Logger (a, l')) = Logger (f a, l ++ l')

instance Monad Logger where
  return :: a -> Logger a
  return = pure
  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  (>>=) (Logger (a, l)) f = Logger (b, l ++ l')
    where (Logger (b, l')) = f a

runLogger :: Logger a -> (a, Log)
runLogger (Logger l) = l

record :: String -> Logger ()
record s = Logger ((), [s])
