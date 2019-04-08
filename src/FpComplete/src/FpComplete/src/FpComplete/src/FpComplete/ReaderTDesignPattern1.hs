{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FpComplete.ReaderTDesignPattern1 where

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

newtype AppOne a = AppOne
  { unAppOne :: ReaderT EnvOne IO a
  } deriving (Functor, Applicative, Monad, MonadReader EnvOne, MonadIO)

runAppOne :: AppOne a -> EnvOne -> IO a
runAppOne app env = runReaderT (unAppOne app) env

-- NOTICE THAT MODIFY TAKES AN ENTIRE ENV VALUE, EVEN THOUGH IT ONLY CARES ABOUT
-- THE TVAR FUNCTION. LETS ADD ANOTHER LAYER OF INDIRECTION TO 
modify :: (MonadReader EnvOne m, MonadIO m) => (Int -> Int) -> m ()
modify f = do
  tVar <- envBalance <$> ask
  liftIO $ atomically $ modifyTVar' tVar f

logSomething :: (MonadIO m, MonadReader EnvOne m) => String -> m ()
logSomething s = do
  logFunc <- envLog <$> ask
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
