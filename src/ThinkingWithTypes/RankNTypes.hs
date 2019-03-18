{-# LANGUAGE RankNTypes #-}
module ThinkingWithTypes.RankNTypes where

-- The function below does not compile. Why? Because what we are telling
-- Haskell here is that applyToFive takes any endomorphism and returns an Int.
-- Of course, that is not true. `not` is an endomorphism but it would not make
-- sense to pass in `not` as the first argument.
-- applyToFive :: (a -> a) -> Int
-- applyToFive f = f 5


-- The key here is that the caller of a polymorphic function gets to decide what
-- type the type variables are instantiated to. We have no guarantee that the
-- caller of `applyToFive` chose a function with the type `Int -> Int`. Haskell
-- is forced to reject the function.
--
-- The problem is that Haskell automatically quantifies type variables. As such,
-- we do not see that the type of the function `id` is truly `id :: forall a. a
-- -> a`.

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- Given the well-typed version of applyToFive, the caller of applyToFive cannot
-- choose a function such as `not`, with the type signature `Bool -> Bool`.

bar :: forall a . (a -> a) -> (Char, Bool)
bar f = ('C', True)
