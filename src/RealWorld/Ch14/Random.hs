module RealWorld.Ch14.Random where

import RealWorld.Ch14.SimpleState
import System.Random

-- In order to make generating random numbers "pure", we have to pass around the
-- generator, and make sure to use the new generator every time. This is
-- error-prone, unless we use the state monad.

type RandomState a = SimpleState StdGen a

getRandom :: Random a => RandomState a
getRandom = do
  stdGen <- get
  let (val, newGen) = random stdGen
  put newGen
  return val

-- The state monad can manage many pieces of state, but we have to bundle all of
-- that up into one container data type. That is what CountedRandom is.
data CountedRandom = CountedRandom { crStdGen :: StdGen, crCount :: Int } deriving Show

getRandom' :: Random a => SimpleState CountedRandom a
getRandom' = do
  countedRandom <- get
  let (a, newGen) = random (crStdGen countedRandom)
      newState = countedRandom { crStdGen = newGen, crCount = oldCount + 1}
      oldCount = crCount countedRandom
  put newState
  return a
