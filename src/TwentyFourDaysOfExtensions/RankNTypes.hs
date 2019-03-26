{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module TwentyFourDaysOfExtensions.RankNTypes where

import Control.Monad.State

-- EXAMPLE: RANDOM GENERATOR
import System.Random

data Player = Player
  { playerName :: String
  , playerPos :: (Double, Double)
  } deriving (Eq, Show, Ord)

-- Must always ask, "How do we read this?" It is another way of saying, "What is
-- this code expressing?".
--
-- A value of type "GenAction m" is a monadic action that will produced a value
-- of type a, for any and all types that have an instance of Random.
type GenAction m
   = forall a. (Random a) =>
                 m a

-- A value of type GenActionR m is a function that takes a tuple of values whose
-- type has an instance of Random, and produces a Monadic action that will
-- return a new value of that same type (with its instance of Random).
type GenActionR m
   = forall a. (Random a) =>
                 (a, a) -> m a

-- Expanding the type of genRandom and simplifying, we get the type signature:
-- genRandom :: (RandomGen g, Random a) => State g a
genRandom :: (RandomGen g) => GenAction (State g)
genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
  liftIO (putStrLn "Generating Random Player...")
  len <- genR (8, 12)
  name <- replicateM len (genR ('a', 'z'))
  x <- genR (-100, 100)
  y <- genR (-100, 100)
  liftIO (putStrLn "Done!")
  return (Player name (x, y))

-- EXAMPLE: SCOTT ENCODING
data List a
  = Cons a
         (List a)
  | Nil

-- Instead of pattern matching, we use continuations
-- This composes better
uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co _ (Cons a xs) = co a xs
uncons co d Nil = d

listNull :: List a -> Bool
listNull = uncons (\_ _ -> False) True

-- The continutation determines what we do if we find that the list is not
-- empty.
listMap :: (a -> b) -> List a -> List b
listMap f = uncons co d
  where
    co = \a xs -> Cons (f a) (listMap f xs)
    d = Nil

-- We can represent a list by what happens when you uncons it; this is called
-- Scott encoding.
newtype ListS a = ListS
  { unconsS :: forall r. (a -> ListS a -> r) -> r -> r
  }

-- We give the list two continuations; what to do if we have a list, and what to
-- do if we don't;
nilS :: ListS a
nilS = ListS (\co ni -> ni)

consS :: a -> ListS a -> ListS a
consS a l = ListS (\co ni -> co a l)

unConsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unConsS' co d (ListS f) = f co d
