{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut15 where

import RIO
import RIO.Partial (read)
import System.Environment (getEnv)

data App = App { appLogFunc :: !LogFunc, appName :: !Utf8Builder }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

class HasName env where
  nameL :: Lens' env Utf8Builder

instance HasName App where
  nameL = lens appName (\x y -> x { appName = y})

main :: IO ()
main = runApp sayHello

-- Generalize runApp so it doesn't mention IO
runApp :: (MonadUnliftIO m, MonadIO m) => RIO App a -> m a
runApp inner = do
  logOptions' <- logOptionsHandle stderr True
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
      let app = App { appLogFunc = logFunc, appName = "Alice" }
      runRIO app inner

-- Generalize sayHello so that it does not mention App
sayHello :: (HasLogFunc env, HasName env) => RIO env ()
sayHello = do
  name <- view nameL
  logDebug $ "Hello, " <> name
