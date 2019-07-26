{-# LANGUAGE InstanceSigs #-}

module Misc.Writer where

import Control.Monad (replicateM_, replicateM)

newtype Writer w a = Writer { unWriter :: (a, w) }

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f w = Writer (b, log)
        where b = f a
              (a, log) = runWriter w

instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure a = Writer (a, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    w <*> w' = Writer (b, l <> l')
        where b = f a
              (f, l) = runWriter w
              (a, l') = runWriter w'

instance Monoid w => Monad (Writer w) where
    return :: a -> Writer w a
    return = pure
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    w >>= k = Writer (b, l <> l')
        where (a, l) = runWriter w
              (b, l') = runWriter (k a)

runWriter :: Writer w a -> (a, w)
runWriter = unWriter

execWriter :: Writer w a -> w
execWriter = snd . runWriter

writer :: (a, w) -> Writer w a
writer = Writer

tell :: w -> Writer w ()
tell w = Writer ((), w)

listen :: Writer w a -> Writer w (a, w)
listen w = Writer (ans, snd ans)
    where ans = runWriter w

pass :: Writer w (a, w -> w) -> Writer w a
pass w = Writer (a, f l)
    where ((a, f), l) = runWriter w

writerComputation :: Writer [String] ()
writerComputation = replicateM_ 10 returnOne
    where returnOne = tell ["ran returnOne"] >> return 1

-- replicateM :: 
