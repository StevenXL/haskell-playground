{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut11 where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr, stdout)

data App = App { appName :: !String, appHandle :: !Handle}

class HasName env where
  nameL :: Lens' env String

class HasHandle env where
  handleL :: Lens' env Handle

-- Change the HasName typeclass from getters / setters to a lens
instance HasName App where
  nameL  = lens appName (\x y -> x {appName = y})

instance HasHandle App where
  handleL = lens appHandle (\x y -> x { appHandle = y})

main :: IO ()
main = do
  let app = App "Alice" stderr
  runRIO app $ do
    sayHello
    switchHandle stdout sayHello
    addLastname sayHello
    sayTime
    sayGoodbye

sayHello :: (HasHandle env, HasName env) => RIO env () -- A
sayHello = do
  name <- view nameL
  say $ "Hello, " ++ name

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The current time is: " ++ show now

sayGoodbye :: (HasHandle env, HasName env) => RIO env ()
sayGoodbye = do
  name <- view nameL
  say $ "Goodbye, " ++ name

-- Use fact that RIO is an instance of MonadReader
-- Look at the type of view :: MonadReader s m => Getting a s a -> m a
say :: HasHandle env => String -> RIO env ()
say s = do
  handle <- view handleL
  liftIO $ hPutStrLn handle s

-- Make use of the fact that RIO is a MonadReader. This means we can set the
-- environment with local.
-- Remember local :: (r -> r) -> m a -> m a
switchHandle :: HasHandle env => Handle -> RIO env () -> RIO env ()
switchHandle h = local (set handleL h)

addLastname :: HasName env => RIO env () -> RIO env ()
addLastname inner = do
  name <- view nameL
  let newName = name ++ " Smith"
  local (set nameL newName) inner
