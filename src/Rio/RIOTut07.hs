{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut07 where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr)

data App = App { appName :: !String, appHandle :: !Handle}

data App2 = App2 { app2Handle :: !Handle, app2FavoriteColor :: !String }

class HasName env where
  getName :: env -> String

class HasHandle env where
  getHandle :: env -> Handle

instance HasName App where
  getName :: App -> String
  getName = appName

instance HasHandle App where
  getHandle :: App -> Handle
  getHandle = appHandle

instance HasHandle App2 where
  getHandle :: App2 -> Handle
  getHandle = app2Handle

main :: IO ()
main = do
  let app = App "Alice" stderr
  runRIO app $ do
    sayHello
    sayTime
    sayGoodbye
  let app2 = App2 stdout "red"
  runRIO app2 sayTime -- we can provide a different environment
                      -- and everything still works because that environemnt
                      -- is an instance of all the typeclasses we need

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
