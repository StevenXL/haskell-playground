{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut12 where

import RIO

-- This module starts the section on loggin
-- Notice that logInfo requires that env is an instance of HasLogFunc. This
-- utilizes everything we learned in the first section regarding getters /
-- setters for our environment. The important thing to remember is that log
-- function to use is stored in the environment, and it requires the argument to
-- be a value of type Utf8Builder.
-- logInfo :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack) => Utf8Builder -> m ()
main :: IO ()
main = runSimpleApp $ do
          logDebug "Debug" -- does not get log unless RIO_VERBOSE = 1
          logInfo "Info"
          logWarn "Warn"
          logError "Error"
