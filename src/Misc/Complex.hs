module Misc.Complex where

-- A simpler solution to the blog post: https://discourse.haskell.org/t/adventures-assembling-records-of-capabilities
-- Maybe it doesn't scale?

import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)

data SimpleCapability = SimpleCapability Int
data ComplexCapability = ComplexCapability SimpleCapability
data Env = Env {simple :: SimpleCapability }

class HasComplex a where
  getComplex :: a -> ComplexCapability

instance HasComplex Env where
  getComplex env = ComplexCapability (simple env)

dummyLogic :: (MonadReader env m, HasComplex env, MonadIO m) => m ()
dummyLogic = do
  env <- ask
  let complex = getComplex env
  liftIO $ putStrLn "running the logic!"
