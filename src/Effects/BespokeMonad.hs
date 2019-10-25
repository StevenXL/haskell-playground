{-# LANGUAGE InstanceSigs #-}

module Effects.BespokeMonad where
  -- Log the value of the accumulator.
  -- Pick an integer uniformly randomly from the half-open interval [0, 10).
  -- Mutate the accumulator by adding the random integer to it.
  -- We need an abstract data type
  -- We need an execution fuction
  -- We need helper functions

-- See: https://github.com/stepchowfun/effects
-- Goal:
-- Solution 1: Bespoke Monad
import Control.Monad (replicateM_)
import System.Random (StdGen, getStdGen, randomR)

-- Abstract data type
data Computation a = Computation
  { unCompution :: StdGen -> Integer -> (StdGen, Integer, String, a)
  }

-- Helper functions
getRandom :: Computation Integer
getRandom =
  Computation $ \g1 i ->
    let (r, g2) = randomR (0, 10) g1
     in (g2, i, "", r)

getAccumulator :: Computation Integer
getAccumulator = Computation $ \g1 i -> (g1, i, "", i)

setAccumulator :: Integer -> Computation ()
setAccumulator i = Computation $ \g1 _ -> (g1, i, "", ())

logOutput :: String -> Computation ()
logOutput s = Computation $ \g1 i -> (g1, i, s, ())

-- Interpreter / Execution Function
runComputation ::
     Computation a -> StdGen -> Integer -> (StdGen, Integer, String, a)
runComputation c s i = unCompution c s i

-- program
program :: Computation ()
program = do
    i <- getAccumulator
    logOutput (show i ++ "\n")
    r <- getRandom
    setAccumulator (r + i)

-- Instances
instance Functor Computation where
  fmap :: (a -> b) -> Computation a -> Computation b
  fmap f m =
    Computation $ \g1 i ->
      let (g2, i2, s, a) = runComputation m g1 i
       in (g2, i2, s, f a)

instance Applicative Computation where
  pure :: a -> Computation a
  pure a = Computation $ \g1 i -> (g1, i, "", a)
  (<*>) :: Computation (a -> b) -> Computation a -> Computation b
  (<*>) c1 c2 =
    Computation $ \g1 i1 ->
      let (g2, i2, s2, f) = runComputation c1 g1 i1
          (g3, i3, s3, a) = runComputation c2 g2 i2
       in (g3, i3, s2 ++ s3, f a)

instance Monad Computation where
  return :: a -> Computation a
  return a = pure a
  (>>=) :: Computation a -> (a -> Computation b) -> Computation b
  (>>=) c k =
    Computation $ \g1 i1 ->
      let (g2, i2, s2, a) = runComputation c g1 i1
          (g3, i3, s3, b) = runComputation (k a) g2 i2
       in (g3, i3, s2 ++ s3, b)

main :: IO ()
main = do
  stdGen <- getStdGen
  print $ runComputation program stdGen 11
  print $ runComputation (program >> program) stdGen 11
  print $ runComputation (replicateM_ 10 program) stdGen 10
