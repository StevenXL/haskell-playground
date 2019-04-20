{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Rio.RIOTut04 where

import RIO
import System.IO (hPutStrLn, stderr)

data App = App { appName :: !String, appHandle :: !Handle}

main :: IO ()
main = do
  let app = App "Alice" stderr
  runRIO app $ do
    sayHello
    sayGoodbye

sayHello :: RIO App ()
sayHello = do
  name <- appName <$> ask
  say $ "Hello, " ++ name

sayGoodbye :: RIO App ()
sayGoodbye = do
  name <- appName <$> ask
  say $ "Goodbye, " ++ name

-- Create a helper function.
say :: String -> RIO App ()
say s = do
  handle <- appHandle <$> ask
  liftIO $ hPutStrLn handle s
