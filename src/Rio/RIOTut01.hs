{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Rio.RIOTut01 where

import RIO

-- Simple app to make sure everything is working
main :: IO ()
main = runSimpleApp $ logInfo "Hello World!"
