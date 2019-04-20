{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut08 where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr, stdout)

-- We want to be able to update a field in our environment. The RIO tutorial
-- recommends a Lenses, but we can first try by just adding another method to
-- the HasHandle typeclass.

data App = App { appName :: !String, appHandle :: !Handle}

class HasName env where
  getName :: env -> String

class HasHandle env where
  getHandle :: env -> Handle
  setHandle :: env -> Handle -> env -- setter method

instance HasName App where
  getName :: App -> String
  getName = appName

instance HasHandle App where
  getHandle :: App -> Handle
  getHandle = appHandle

  setHandle :: App -> Handle -> App
  setHandle app h = app { appHandle = h}

main :: IO ()
main = do
  let app = App "Alice" stderr
  runRIO app $ do
    sayHello
    switchHandle stdout sayHello -- we are only changing the environment locally
    sayTime
    sayGoodbye

sayHello :: (HasHandle env, HasName env) => RIO env () -- A
sayHello = do
  name <- getName <$> ask -- B
  say $ "Hello, " ++ name

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The current time is: " ++ show now

sayGoodbye :: (HasHandle env, HasName env) => RIO env ()
sayGoodbye = do
  name <- getName <$> ask
  say $ "Goodbye, " ++ name

say :: HasHandle env => String -> RIO env ()
say s = do
  handle <- getHandle <$> ask
  liftIO $ hPutStrLn handle s

switchHandle :: HasHandle env => Handle -> RIO env () -> RIO env ()
switchHandle h inner = do
  app <- ask
  let newEnv = setHandle app h
  runRIO newEnv inner
