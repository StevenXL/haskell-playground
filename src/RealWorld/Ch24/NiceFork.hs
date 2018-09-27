module RealWorld.Ch24.NiceFork where

import Control.Exception (SomeException, try)
import Control.Concurrent (MVar, ThreadId, newMVar, modifyMVar, newEmptyMVar, forkIO, putMVar, readMVar)
import Data.Map (Map)
import qualified Data.Map as M

data ThreadStatus  = Running | Finished | Threw SomeException deriving Show

newtype ThreadManager = Mgr (MVar (Map ThreadId (MVar ThreadStatus)))

newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) action = do
    modifyMVar mgr $ \map -> do
        state <- newEmptyMVar
        threadId <- forkIO $ do
            result <- try action
            putMVar state (either Threw (const Finished) result)
        return (M.insert threadId state map, threadId)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus = undefined


waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor = undefined

waitAll :: ThreadManager -> IO ()
waitAll = undefined
