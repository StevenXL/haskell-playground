{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Rio.RIOTut13 where

import RIO

-- Our own applications should be able to define their own logging function.
data App = App { appLogFunc :: !LogFunc , appName :: Utf8Builder }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y})

runApp :: RIO App a -> IO a
runApp inner = runSimpleApp $ do
    logFunc <- view logFuncL -- here, we are stealing the logging function from SimpleApp environment.
    let app = App { appLogFunc = logFunc , appName = "Alice" }
    runRIO app inner

sayHello :: RIO App ()
sayHello = do
    name <- view $ to appName
    logInfo $ "Hello, " <> name

main :: IO ()
main = runApp sayHello
