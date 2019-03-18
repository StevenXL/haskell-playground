{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module ThinkingWithTypes.Cont where

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f c = Cont newContinutation
      where newContinutation callback = unCont c (callback . f)

instance Applicative Cont where
    pure :: a -> Cont a
    pure a = Cont c
        where c callback = callback a

    (<*>) :: Cont (a -> b) -> Cont a -> Cont b
    (<*>) contAB contA = Cont newCont
        where newCont callBack = undefined

-- unCont contAB :: forall r. ((a -> b) -> r) -> r
-- unCont contA :: forall r. (a -> r) -> r
-- callback :: (b -> r)

-- -> r (a -> r)
-- so now we have a function from a to r, b to r,
-- f :: a -> (a -> b -> r)

-- fmap once to get access to the callback!
-- Once we have access to that callback, we can manipulate it as a value!
