{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkingWithTypes.FirstClassFamilies01 where

import Prelude hiding (fst)
import qualified Prelude as P

increase :: Int -> Int
increase = ((+) 1)

-- DEFUNCTIONALIZATION: EXAMPLE 1
-- We start off with a polymorphic function:
fst :: (a, b) -> a
fst (a, b) = a

-- We use a technique called defunctionalization to replace a polymorphic
-- function with a data type; in the context of defunctionalization, the data
-- type is known as a label
data Fst a b =
  Fst (a, b)

-- AND a typeclass
-- This typeclass is a multi-parameter typeclass with functional dependencies
class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval :: Fst a b -> a
  eval (Fst (a, b)) = a

-- DEFUNCTIONALIZATION: EXAMPLE 2
-- We start off with a polymorphic function:
mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (a:as) = f a : mapList f as

-- We create a data type; again, MapList is our label.
data MapList a b =
  MapList (a -> b)
          [a]

instance (Eval b t) => Eval (MapList a b) [t] where
  eval :: MapList a b -> [t]
  eval (MapList _ []) = []
  eval (MapList f (a:as)) = eval (f a) : eval (MapList f as)
