{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut09 where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr, stdout)

data App = App { appName :: !String, appHandle :: !Handle}

class HasName env where
  getName :: env -> String

-- Replace the HasHandle getter / setter typeclass with a single lens.
class HasHandle env where
  handleL :: Lens' env Handle

instance HasName App where
  getName :: App -> String
  getName = appName

-- The creation of this lens is pretty simple. We simply feed a getter function
-- and a setter function to the lens function.
instance HasHandle App where
  handleL = lens appHandle (\x y -> x { appHandle = y})

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

-- use the view function with a lens to view a part of a data structure
say :: HasHandle env => String -> RIO env ()
say s = do
  handle <- view handleL <$> ask
  liftIO $ hPutStrLn handle s

-- use the set function with a lens to set part of a data structure
switchHandle :: HasHandle env => Handle -> RIO env () -> RIO env ()
switchHandle h inner = do
  app <- ask
  let newEnv = set handleL h app
  runRIO newEnv inner
