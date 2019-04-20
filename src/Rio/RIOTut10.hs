{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut10 where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr, stdout)

data App = App { appName :: !String, appHandle :: !Handle}

class HasName env where
  getName :: env -> String

class HasHandle env where
  handleL :: Lens' env Handle

instance HasName App where
  getName :: App -> String
  getName = appName

instance HasHandle App where
  handleL = lens appHandle (\x y -> x { appHandle = y})

main :: IO ()
main = do
  let app = App "Alice" stderr
  runRIO app $ do
    sayHello
    switchHandle stdout sayHello
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

-- Use fact that RIO is an instance of MonadReader
-- Look at the type of view :: MonadReader s m => Getting a s a -> m a
say :: HasHandle env => String -> RIO env ()
say s = do
  handle <- view handleL
  liftIO $ hPutStrLn handle s

-- Make sure of the fact that RIO is a MonadReader. This means we can set the
-- environment with local.
-- Remember local :: (r -> r) -> m a -> m a
switchHandle :: HasHandle env => Handle -> RIO env () -> RIO env ()
switchHandle h = local (set handleL h)
