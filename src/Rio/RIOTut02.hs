{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Rio.RIOTut02 where

import RIO
import Prelude (putStrLn)

main :: IO ()
main = do
  let name = "Alice"
  sayHello name
  sayGoodbye name

sayHello :: String -> IO ()
sayHello name = putStrLn $ "Hello, " ++ name

sayGoodbye :: String -> IO ()
sayGoodbye name = putStrLn $ "Goodbye, " ++ name
