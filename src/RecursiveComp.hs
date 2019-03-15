module RecursiveComp where

import Control.Monad.Identity
import Control.Monad.State

type M = StateT Int Identity

recursiveFunction :: [a] -> Int
recursiveFunction [] = 0
recursiveFunction (_:rest) = 1 + recursiveFunction rest

recursiveComputation :: (Num a, Eq a) => [a] -> M Int
recursiveComputation [] = return 0
recursiveComputation (t:ts) = do
  when (t == 4) (modify (+1))
  rest <- recursiveComputation ts
  return (1 + rest)



main :: IO ()
main = do
  print $ runStateT (recursiveComputation [1..10]) 0
  print $ runStateT (recursiveComputation [4, 4, 3]) 0
