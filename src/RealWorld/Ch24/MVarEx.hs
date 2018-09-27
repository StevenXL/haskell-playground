module RealWorld.Ch24.MVarEx where

import Control.Concurrent (forkIO, takeMVar, putMVar, newEmptyMVar)

main :: IO ()
main = communicate

communicate :: IO ()
communicate = do
  mVar <- newEmptyMVar -- create an MVar; this is how the threads will communicate
  forkIO $ do
    str <- takeMVar mVar -- if MVar is empty, this will block / sleep
    putStrLn str
    putMVar mVar "Done"  -- Important to read docs of putMVar and takeMVar; contain semantics of "single-awake"
  putStrLn "Sending Msg"
  putMVar mVar "Message from main to thread"
  takeMVar mVar
  return ()
