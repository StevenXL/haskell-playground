{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FpComplete.ReaderTDesignPattern2 where

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

-- CHANGE 1: Change modify so that it is NOT hard-coded to expect a specific
-- environment. Instead, the environment that it does get is an instance of
-- HasBalance, which means we can apply the getBalance method to the
-- environment.
modify :: (MonadReader env m, MonadIO m, HasBalance env) => (Int -> Int) -> m ()
modify f = do
  tVar <- getBalance <$> ask
  liftIO $ atomically $ modifyTVar' tVar f

-- CHANGE 1: Change logSomething so that it doesn't require a specific
-- environment. Instead, it requires that the environment implements a specific
-- typeclass.
logSomething :: (MonadIO m, MonadReader env m, HasLog env) => String -> m ()
logSomething s = do
  logFunc <- getLog <$> ask
  liftIO $ logFunc s

appOne :: AppOne ()
appOne = do
  logSomething "increasing balance"
  modify (+ 1)
  logSomething "done"

mainOne :: IO ()
mainOne = do
  tVar <- newTVarIO 0
  runAppOne appOne (EnvOne print tVar)
  newBal <- readTVarIO tVar
  print newBal
