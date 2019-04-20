{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Rio.RIOTut05 where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr)

data App = App { appName :: !String, appHandle :: !Handle}

-- Notice that sayTime requires an entire environment, even though it doesn't
-- care about the appName field of our environment. The say applies for say.

main :: IO ()
main = do
  let app = App "Alice" stderr
  runRIO app $ do
    sayHello
    sayTime
    sayGoodbye

sayHello :: RIO App ()
sayHello = do
  name <- appName <$> ask
  say $ "Hello, " ++ name

sayTime :: RIO App ()
sayTime = do
  now <- getCurrentTime
  say $ "The current time is: " ++ show now

sayGoodbye :: RIO App ()
sayGoodbye = do
  name <- appName <$> ask
  say $ "Goodbye, " ++ name

say :: String -> RIO App ()
say s = do
  handle <- appHandle <$> ask
  liftIO $ hPutStrLn handle s
