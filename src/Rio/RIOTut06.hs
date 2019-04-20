{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut06 where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr)

-- GOAL is for our actions to only require the part of the environment they care
-- about.

data App = App { appName :: !String, appHandle :: !Handle}

-- Step 1: Define the interface for the environment.
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

main :: IO ()
main = do
  let app = App "Alice" stderr
  runRIO app $ do
    sayHello
    sayTime
    sayGoodbye

-- Step 2: (A) Change our functions to constrain the environment to only the pieces
-- they care about, and (B) utilize the typeclass method / interface for the
-- env.
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
