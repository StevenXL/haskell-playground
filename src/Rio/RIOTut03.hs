{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Rio.RIOTut03 where

import RIO
import Prelude (putStrLn)

type Name = String

-- In 02, we passed around the environment "Alice" manually.
-- Here, we are using the fact that RIO is a ReaderT and passing in the
-- environment implicitly.
main :: IO ()
main = do
  let name = "Alice"
  runRIO name $ do
    sayHello
    sayGoodbye

sayHello :: RIO Name ()
sayHello = do
  name <- ask
  liftIO $ putStrLn $ "Hello, " ++ name

sayGoodbye :: RIO Name ()
sayGoodbye = do
  name <- ask
  liftIO $ putStrLn $ "Goodbye, " ++ name
