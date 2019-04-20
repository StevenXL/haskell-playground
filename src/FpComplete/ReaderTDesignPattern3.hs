{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FpComplete.ReaderTDesignPattern3 where

import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.Reader
  ( MonadIO
  , MonadReader(..)
  , ReaderT
  , liftIO
  , runReaderT
  )
import Control.Monad.STM (atomically)

data EnvOne = EnvOne
  { envLog :: (String -> IO ())
  , envBalance :: TVar Int
  }

class HasBalance a where
  getBalance :: a -> TVar Int

instance HasBalance EnvOne where
  getBalance = envBalance

class HasLog a where
  getLog :: a -> (String -> IO ())

instance HasLog EnvOne where
  getLog = envLog

newtype AppOne a = AppOne
  { unAppOne :: ReaderT EnvOne IO a
  } deriving (Functor, Applicative, Monad, MonadReader EnvOne, MonadIO)

runAppOne :: AppOne a -> EnvOne -> IO a
runAppOne app env = runReaderT (unAppOne app) env

-- CHANGE 1: Create a new typeclass that does not have a MonadIO constraint.
-- There are an infinite number of monads that can be an instance of
-- MonadBalance.
class (Monad m) =>
      MonadBalance m
  where
  modifyBalance :: (Int -> Int) -> m ()

instance MonadBalance AppOne where
  modifyBalance f = do
    tVar <- getBalance <$> ask
    liftIO $ atomically $ modifyTVar' tVar f

class Monad m =>
      MonadLogger m
  where
  logSomething :: String -> m ()

instance MonadLogger AppOne where
  logSomething s = do
    logFunc <- getLog <$> ask
    liftIO $ logFunc s

modify :: MonadBalance m => (Int -> Int) -> m ()
modify = modifyBalance

appOne :: AppOne ()
appOne = do
  logSomething "increasing balance"
  modifyBalance (+ 1)
  logSomething "done"

mainOne :: IO ()
mainOne = do
  tVar <- newTVarIO 0
  runAppOne appOne (EnvOne print tVar)
  newBal <- readTVarIO tVar
  print newBal
